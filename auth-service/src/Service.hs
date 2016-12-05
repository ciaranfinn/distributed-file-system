{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Service where

import           Database.Persist.Postgresql          (Entity(..),ConnectionPool,selectList,fromSqlKey,insert)
import           Control.Monad.Except
import           Control.Monad.Reader                 (MonadIO, MonadReader,ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           AuthApi                              (AuthServiceApi,ResponseData(..))
import           Models                               (dbAction,Credentials)
import           Servant


newtype App a = App
    { runApp :: ReaderT ConnectionPool (ExceptT ServantErr IO) a
    } deriving ( Functor, Applicative, Monad, MonadReader ConnectionPool,
                 MonadError ServantErr, MonadIO)

app :: ConnectionPool -> Application
app cp = serve appApi (appToServer cp)

appToServer :: ConnectionPool -> Server AuthServiceApi
appToServer cp = enter (convertApp cp) authService

convertApp :: ConnectionPool -> App :~> ExceptT ServantErr IO
convertApp cp = Nat (flip runReaderT cp . runApp)

appApi :: Proxy AuthServiceApi
appApi = Proxy

authService :: ServerT AuthServiceApi App
authService = verify


-- Service Actions --

verify :: Credentials -> App ResponseData
verify = do
    return ResponseData {status = "a" }
