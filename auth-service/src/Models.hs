{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Control.Monad.Reader
import           Database.Persist.Postgresql
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    email String
    password String
    UniqueName email
    deriving Show
|]

dbAction :: (MonadReader ConnectionPool m, MonadIO m) => SqlPersistT IO b -> m b
dbAction query = do
    pool <- ask
    liftIO $ runSqlPool query pool

dbMigration :: SqlPersistT IO ()
dbMigration = runMigration migrateAll
