{-# LANGUAGE TypeFamilies #-}
module ICFPC2024.API.RateLimit (
  checkRateLimit,
) where

import ICFPC2024.Config ( Config (..), RateLimitConfig (..) )
import ICFPC2024.Database ( TeamId )
import ICFPC2024.Environment ( ServerM, getConfig, getRedisPool, logM, LogPath (..), toLogStr )

import Network.Wai ( Response, responseBuilder )
import Network.HTTP.Types ( status429 )

import Data.Int ( Int64 )
import Data.Time.Clock.System
import qualified Data.ByteString.Char8 as BS8

import Database.Redis.Schema
import Database.Persist.Sql ( fromSqlKey )

import Control.Monad.Trans ( lift )

-- | Redis key for team + time bucket
data RateLimitKey = RateLimitKey TeamId Int64

instance Ref RateLimitKey where
  type ValueType RateLimitKey = Int
  toIdentifier (RateLimitKey tid time) =
    SviTopLevel $ colonSep ["ratelimit", BS8.pack (show $ fromSqlKey tid), BS8.pack (show time)]

-- | Check the rate limit, e.g. that a single team does not overload the server
checkRateLimit :: TeamId -> ServerM dt Response -> ServerM dt Response
checkRateLimit tid cont = do
  opts <- rateLimit <$> getConfig
  pool <- getRedisPool
  now <- lift $ systemSeconds <$> getSystemTime
  -- by using integer division, we simply round down to a bucket number, so that
  -- for the length of 'timeFrameSec' seconds the result is the same
  let bucket = now `div` timeFrameSec opts
  let key = RateLimitKey tid bucket
  requests <- run pool $ incrementBy key 1
  run pool $ setTTL key (TTLSec $ toInteger $ timeFrameSec opts)
  if requests > maxRequests opts
    then do
    logM RateLimit $ "Team " <> toLogStr tid <> " reached the rate limit"
    pure $ responseBuilder status429 [] ""
    else cont
