{-# LANGUAGE FlexibleContexts #-}
module ICFPC2024.ObjectStorage (
  Storage (..),
  storeMessage,
  readMessage,
) where

import ICFPC2024.Database ( MessageHash (..) )
import ICFPC2024.Environment ( ServerM, getOSEnv )

import Data.Digest.Pure.MD5 ( md5 )
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS8

import System.FilePath ( (</>) )
import System.Directory ( createDirectoryIfMissing )

import Control.Monad.Trans ( lift )

#ifdef PRODUCTION
import ICFPC2024.Environment ( getConfig )
import ICFPC2024.Config ( storageConfig, StorageConfig (..) )

import qualified Amazonka as AWS
import qualified Amazonka.S3 as AWS
import qualified Amazonka.S3.GetObject as AWS

import Control.Lens ( (^.) )
import Control.Monad.Trans.Resource ( ResourceT )

import Data.String ( fromString )
import Data.Conduit.Combinators ( sinkLazy )
import Data.Typeable ( Typeable )
#else
import Data.Void ( absurd )
#endif

-- | Different storage directories
data Storage
  = StorageRequest
  | StorageResponse

-- | Directory names of storage locations
dirName :: Storage -> FilePath
dirName StorageRequest  = "request"
dirName StorageResponse = "response"

#ifdef PRODUCTION
-- | Bucket name of storage locations
bucketName :: Storage -> ServerM dt AWS.BucketName
bucketName StorageRequest = requestBucket . storageConfig <$> getConfig
bucketName StorageResponse = responseBucket . storageConfig <$> getConfig
#endif

-- | Store a message on the object storage, and return
--   its hash
storeMessage :: Storage -> ByteString -> ServerM dt MessageHash
storeMessage st msg = do
  let hash = md5 msg
  env <- getOSEnv
  case env of
    Left baseDir -> do
      let dir = baseDir </> dirName st
      lift $ createDirectoryIfMissing True dir
      let fn = dir </> show hash
      lift $ BS8.writeFile fn msg
#ifdef PRODUCTION
    Right awsEnv -> do
      bucket <- bucketName st
      let key = fromString $ show hash
      sendServerM awsEnv (AWS.newPutObject bucket key $ AWS.toBody msg) (const $ pure ())
#else
    Right v -> absurd v
#endif
  pure $ MessageHash hash

-- | Get a message from the object storage
readMessage :: Storage -> MessageHash -> ServerM dt ByteString
readMessage st (MessageHash hash) = do
  env <- getOSEnv
  case env of
    Left baseDir -> do
      let fn = baseDir </> dirName st </> show hash
      lift $ BS8.readFile fn
#ifdef PRODUCTION
    Right awsEnv -> do
      bucket <- bucketName st
      let key = fromString $ show hash
      sendServerM awsEnv (AWS.newGetObject bucket key) $ \resp -> do
        -- Force the result to make sure we get the body before the connection is closed
        bs <- (resp^.AWS.getObjectResponse_body) `AWS.sinkBody` sinkLazy
        BS8.length bs `seq` pure bs
#else
    Right v -> absurd v
#endif

#ifdef PRODUCTION
-- | Send an AWS request to the server inside our ServerM dt monad
sendServerM :: (Typeable a, Typeable (AWS.AWSResponse a), AWS.AWSRequest a) =>
  AWS.Env -> a -> (AWS.AWSResponse a -> ResourceT (ServerM dt) b) -> ServerM dt b
sendServerM env op cont = do
  AWS.runResourceT $ AWS.send env op >>= cont
#endif
