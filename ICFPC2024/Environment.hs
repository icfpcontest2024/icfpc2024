{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
module ICFPC2024.Environment (
  ServerM,
  runServerM,
  runServerMBG,
  getConfig,
  getDBPool,
  getStaticData,
#ifdef PRODUCTION
  getRedisPool,
#endif
  getOSEnv,
  logM,
  LogPath (..),
  ToLogStr (..),
) where

import ICFPC2024.Config as Config ( Config (..), LogConfig (..) )
import ICFPC2024.Database.Connection ( withDBPool, DBPool )
import ICFPC2024.Static ( readStaticData, StaticData )
import ICFPC2024.ObjectStorage.Environment

import Data.Yaml ( decodeFileThrow )
import Data.Type.Bool ( If )

import Control.Monad.Trans ( lift )
import Control.Monad.Reader ( ReaderT(..), asks )

import System.FilePath ( (</>), (<.>) )
import System.Directory ( createDirectoryIfMissing )
import System.Log.FastLogger as Log

#ifdef PRODUCTION
import Database.Redis.Schema ( Pool, DefaultInstance, connect )

import ICFPC2024.Config ( RedisConfig (..) )
#endif

-- | The server monad stack, so that the config and database connection are available everywhere
type ServerM dt = ReaderT (ServerState dt) IO

-- | All values we keep in memory in the server, some connections and some static info
data ServerState (dt :: Bool) = ServerState
  { ssConf :: Config
  , ssDBPool :: DBPool
#ifdef PRODUCTION
  , ssRedisPool :: Pool DefaultInstance
#endif
  , ssStaticData :: If dt StaticData ()
  , ssObjectStorage :: ObjectStorageEnv
  , ssLogger :: LogPath -> TimedFastLogger
  }

-- | Read the YAML config file, open the database connection, and run the server connection.
runServerM :: FilePath -> ServerM 'True a -> IO a
runServerM fp cont = initServerM fp $ \conf ss -> do
  dt <- readStaticData conf
  runReaderT cont $ ss { ssStaticData = dt }

-- | Run a background server job, for which the static puzzle data is not needed
runServerMBG :: FilePath -> ServerM 'False a -> IO a
runServerMBG fp cont = initServerM fp $ const $ runReaderT cont

-- | Help for running the two different types for ServerM
initServerM :: FilePath -> (Config -> ServerState 'False -> IO a) -> IO a
initServerM fp cont = do
  conf <- decodeFileThrow fp
#ifdef PRODUCTION
  redis <- connect (connectionString $ redisConfig conf) (poolSize $ redisConfig conf)
#endif
  osenv <- makeObjectStorageEnv (storageConfig conf)
  withDBPool (dbConfig conf) $ \pool ->
    withLogger (logging conf) $ \logger ->
    cont conf $ ServerState
      { ssConf = conf
      , ssDBPool = pool
#ifdef PRODUCTION
      , ssRedisPool = redis
#endif
      , ssStaticData = ()
      , ssObjectStorage = osenv
      , ssLogger = logger
      }

-- | Get the config file from the environment
getConfig :: ServerM dt Config
getConfig = asks ssConf

-- | Get the database pool from the environment
getDBPool :: ServerM dt DBPool
getDBPool = asks ssDBPool

-- | Get the static puzzle data from the config
getStaticData :: ServerM 'True StaticData
getStaticData = asks ssStaticData

#ifdef PRODUCTION
-- | Get access to the Redis pool
getRedisPool :: ServerM dt (Pool DefaultInstance)
getRedisPool = asks ssRedisPool
#endif

-- | Get the in-memory environment for the object storage
getOSEnv :: ServerM dt ObjectStorageEnv
getOSEnv = asks ssObjectStorage

-- | Different log paths
data LogPath
  = Errors
  | Puzzles
  | RateLimit
  deriving ( Show )

-- | Init the logging
withLogger :: LogConfig -> ((LogPath -> TimedFastLogger) -> IO a) -> IO a
withLogger cfg cont = do
  case cfg of
    Config.LogFile {..} -> createDirectoryIfMissing True logDirectory
    _ -> pure ()
  tc <- newTimeCache "%F %T"
  let spec lp = case cfg of
        Config.LogStdout    -> Log.LogStdout
        Config.LogFile {..} -> Log.LogFile (FileLogSpec (logDirectory </> show lp <.> "log") logFileSize logBackupNumber)
  -- A big ugly, but we make a logger for each of the log paths. We could also enumerate them, but then we need to
  -- explicitly clean up, and we have a small amount anyway
  withTimedFastLogger tc (spec Errors defaultBufSize) $ \log1 ->
    withTimedFastLogger tc (spec Puzzles defaultBufSize) $ \log2 ->
    withTimedFastLogger tc (spec RateLimit defaultBufSize) $ \log3 -> do
    let logf Errors  = log1
        logf Puzzles = log2
        logf RateLimit = log3
    cont logf

-- | Log a message
logM :: LogPath -> LogStr -> ServerM dt ()
logM lp msg = do
  logf <- asks ssLogger
  lift $ logf lp $ \time -> "[" <> toLogStr (show lp) <> "] " <> toLogStr time <> " " <> msg <> "\n"
