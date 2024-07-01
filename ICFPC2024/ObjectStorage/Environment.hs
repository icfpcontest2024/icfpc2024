{-# LANGUAGE NamedFieldPuns #-}
module ICFPC2024.ObjectStorage.Environment (
  makeObjectStorageEnv,
  ObjectStorageEnv,
) where

import ICFPC2024.Config ( StorageConfig (..) )

#ifdef PRODUCTION
import Control.Lens ( (&), (.~) )

import qualified Amazonka as AWS
import qualified Amazonka.Auth as AWS
import qualified Amazonka.S3 as S3

import qualified Data.ByteString.Char8 as BS8
#else
import Data.Void ( Void )
#endif

-- | In memory environment for the object storage
type ObjectStorageEnv = Either FilePath
#ifdef PRODUCTION
  AWS.Env
#else
  Void
#endif

-- | Create the object storage environment, this should be called once
makeObjectStorageEnv :: StorageConfig -> IO ObjectStorageEnv
makeObjectStorageEnv StorageLocal{directory} = pure $ Left directory
#ifdef PRODUCTION
makeObjectStorageEnv conf = do
  let setEndpoint secure host port s@AWS.Service {AWS.endpoint} =
        s { AWS.endpoint = \r -> (endpoint r)
            { AWS.secure = secure
            , AWS.host = host
            , AWS.port = port
            , AWS.scope = BS8.pack $ s3Region conf
            }
          }
      customS3 = S3.defaultService
        & setEndpoint True (BS8.pack $ s3Endpoint conf) 443
        & AWS.service_s3AddressingStyle .~ AWS.S3AddressingStyleVirtual

  env' <- AWS.newEnv $ pure .
          AWS.fromKeys (s3Key conf) (s3Secret conf)
  pure $ Right $ AWS.configureService customS3 env'
#endif
