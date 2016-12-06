{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Service where

import           Database.Persist.Postgresql          (Entity(..),ConnectionPool,selectList,fromSqlKey,insertBy,insert)
import           Control.Monad.Except
import           Control.Monad.Reader                 (MonadIO, MonadReader,ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           AuthApi                              (AuthServiceApi,ResponseData(..))
import           Models
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
authService = createUser :<|> verify


-- Service Actions --
createUser :: User -> App ResponseData
createUser user = do
    let email = userEmail user
    let password = userPassword user
    newUser <- dbAction (insertBy (User email password))
    case newUser of
        Left _ ->
            throwError err500 { errBody = "User exists" }
        Right user ->
            pure ResponseData {status = show (fromSqlKey user) }


verify :: User -> App ResponseData
verify u = do
    newAct <- dbAction (insert (User (userEmail u) (userPassword u)))
    pure ResponseData {status = "a" }
