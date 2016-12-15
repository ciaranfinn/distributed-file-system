{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Service (startApp) where

import Api                                 (API,Subscriber(..))
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Database.Redis
import Control.Monad.IO.Class
import Control.Monad.Reader                 (MonadIO, MonadReader,ReaderT(..), runReaderT,ask)


startApp :: IO ()
startApp = do
  conn <- checkedConnect defaultConnectInfo
  run 8080 $ app conn

type App = ReaderT Connection Handler

app :: Connection -> Application
app conn = serve appApi (appToServer conn)

appToServer :: Connection -> Server API
appToServer conn = enter (runReaderTNat conn) authService

appApi :: Proxy API
appApi = Proxy

authService :: ServerT API App
authService = register


myRunRedis :: (MonadReader Connection m, MonadIO m) => Redis a -> m a
myRunRedis query = do
    conn <- ask
    liftIO $ runRedis conn query

register :: Subscriber -> App Subscriber
register arg = do
  myRunRedis $ do
     set "hello" "hello"
     set "world" "world"
     hello <- get "hello"
     world <- get "world"
     liftIO $ print (hello,world)
  pure Subscriber{ip_address = "yo", port = 10, message = "a", service_type = "a"}
