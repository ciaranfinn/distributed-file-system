{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Service
import           Database.Persist.Postgresql (runSqlPool,createPostgresqlPool)
import           Network.Wai.Handler.Warp    (run)
import           Models                      (dbMigration)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           Data.ByteString              (ByteString)
import           Data.ByteString.Char8        (pack)




serviceRunPort = 8000

main :: IO ()
main = do
  ip <- postgresDbIp
  let connection = pack ("host=" ++ ip ++ " dbname=postgres user=docker port=5433")
  pool <- runStderrLoggingT $ createPostgresqlPool connection 10
  runSqlPool dbMigration pool
  run serviceRunPort $ logStdout $ app pool


postgresDbIp :: IO String
postgresDbIp = defEnv "POSTGRESQL_IP" id "localhost" True


defEnv :: Show a
              => String
              -> (String -> a)
              -> a
              -> Bool
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> return def
