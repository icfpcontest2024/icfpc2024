module ICFPC2024.Database.Connection where

import ICFPC2024.Config ( DBConfig(..) )

import Data.Pool ( Pool )
#ifdef PRODUCTION
import Data.ByteString.Char8 ( pack)
import Database.Persist.MySQL
import Database.MySQL.Connection ( utf8mb4_unicode_ci )
#else
import Database.Persist.Sqlite
#endif

import Control.Monad.Trans ( lift )
import Control.Monad.Logger ( runNoLoggingT )

type DBPool = Pool SqlBackend

-- | Based on the config file, construct a database pool
withDBPool :: DBConfig -> (DBPool -> IO a) -> IO a
#ifdef PRODUCTION
withDBPool conf f = runNoLoggingT (withMySQLPool ci (dbPoolSize conf) (lift . f)) where
  ci = setMySQLConnectInfoPort (fromInteger $ dbPort conf) $
    setMySQLConnectInfoCharset utf8mb4_unicode_ci $
    mkMySQLConnectInfo
    (dbHost conf)
    (pack $ dbUser conf)
    (pack $ dbPassword conf)
    (pack $ dbDatabase conf)
#else
withDBPool conf f = runNoLoggingT (withSqlitePoolInfo ci (dbPoolSize conf) (lift . f)) where
  ci = mkSqliteConnectionInfo $ dbConnectionString conf
#endif
