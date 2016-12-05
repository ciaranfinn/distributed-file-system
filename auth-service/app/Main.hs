{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Service
import           Database.Persist.Postgresql (runSqlPool,createPostgresqlPool)
import           Network.Wai.Handler.Warp    (run)
import           Models                      (dbMigration)
import           Control.Monad.Logger        (runStderrLoggingT)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

connStr = "host=localhost dbname=postgres user=docker port=5433"
serviceRunPort = 8000

main :: IO ()
main = do
  pool <- runStderrLoggingT $ createPostgresqlPool connStr 10
  runSqlPool dbMigration pool
  run serviceRunPort $ logStdout $ app pool
