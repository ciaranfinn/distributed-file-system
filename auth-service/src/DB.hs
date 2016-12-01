{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DB (runMigrate,User) where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger    (runStderrLoggingT)

connStr = "host=localhost dbname=postgres user=docker port=5433"


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    password String Maybe
    deriving Show
|]

runMigrate :: IO ()
runMigrate = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigrationUnsafe migrateAll
