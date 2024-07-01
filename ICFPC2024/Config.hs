module ICFPC2024.Config where

import Data.Word ( Word64 )
import Data.Time ( UTCTime )
import Data.Aeson
import GHC.Generics ( Generic )

#ifdef PRODUCTION
import Data.Int ( Int64 )

import Amazonka ( AccessKey, SecretKey )
import Amazonka.S3 ( BucketName )

import Control.Applicative ( (<|>) )
#else
import Data.Text ( Text )
#endif

-- * Configuration settings

data Config = Config
  { dbConfig :: DBConfig
#ifdef PRODUCTION
  , redisConfig :: RedisConfig
  -- Should the master evaluate expressions directly? For local use only
  , localWorker :: Bool
  -- Do we use a seperate worker queue for requests that time out?
  , timeoutWorker :: Bool
  , rateLimit :: RateLimitConfig
#endif
  , httpPort :: Int
  , maxBodySize :: Word64
  , maxRuntimeSec :: Int
  , staticFileDir :: FilePath
  , storageConfig :: StorageConfig
  , contestTimes :: TimeConfig
  , logging :: LogConfig
  } deriving ( Generic )

instance FromJSON Config

data DBConfig = DBConfig
  { dbPoolSize :: Int
#ifdef PRODUCTION
  , dbHost     :: String
  , dbPort     :: Integer
  , dbUser     :: String
  , dbPassword :: String
  , dbDatabase :: String
#else
  , dbConnectionString :: Text
#endif
  } deriving ( Generic )
instance FromJSON DBConfig

#ifdef PRODUCTION
data RedisConfig = RedisConfig
  { connectionString :: String
  , poolSize         :: Int
  } deriving ( Generic )

instance FromJSON RedisConfig

data RateLimitConfig = RateLimitConfig
  { timeFrameSec :: Int64
  , maxRequests  :: Int
  } deriving ( Generic )

instance FromJSON RateLimitConfig
#endif

data TimeConfig = TimeConfig
  { contestStart :: UTCTime
  , lightningEnd :: UTCTime
  , contestEnd   :: UTCTime
  , freezeMarginHour :: Int -- how many hours before/after the (lightning) end should we freeze
  } deriving ( Generic )

instance FromJSON TimeConfig

data StorageConfig
  = StorageLocal { directory :: FilePath }
#ifdef PRODUCTION
  | StorageS3
    { s3Key    :: AccessKey
    , s3Secret :: SecretKey
    , s3Region :: String
    , s3Endpoint :: String
    , requestBucket :: BucketName
    , responseBucket :: BucketName
    }
#endif
    deriving ( Generic )

instance FromJSON StorageConfig where
  parseJSON v =
#ifdef PRODUCTION
    withObject "StorageS3"
    (\o -> StorageS3 <$> o .: "s3Key"
                     <*> o .: "s3Secret"
                     <*> o .: "s3Region"
                     <*> o .: "s3Endpoint"
                     <*> o .: "requestBucket"
                     <*> o .: "responseBucket"
    ) v
    <|>
#endif
    withObject "StorageLocal" (\o -> StorageLocal <$> o .: "directory") v

data LogConfig
  = LogStdout
  | LogFile
    { logDirectory :: FilePath
    , logFileSize :: Integer
    , logBackupNumber :: Int
    } deriving ( Generic )

instance FromJSON LogConfig where
  parseJSON (String "stdout") = pure LogStdout
  parseJSON v =
    withObject "LogConfig"
    (\o -> LogFile <$> o .: "directory"
                   <*> o .: "fileSize"
                   <*> o .: "backupNumber"
    ) v
