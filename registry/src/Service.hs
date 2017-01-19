{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Service (startApp) where

import RegistryAPI                          (APIreg,Subscriber(..),RResponse(..))
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Database.Redis
import Control.Monad.IO.Class
import Control.Monad.Reader                 (MonadIO, MonadReader,ReaderT(..), runReaderT,ask)
import Data.ByteString                      (ByteString)
import Data.ByteString.Char8                (pack)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.UTF8                 (toString,fromString)


startApp :: IO ()
startApp = do
  conn <- checkedConnect defaultConnectInfo
  run 8080 $ app conn

type App = ReaderT Connection Handler

app :: Connection -> Application
app conn = serve appApi (appToServer conn)

appToServer :: Connection -> Server APIreg
appToServer conn = enter (runReaderTNat conn) registryService

appApi :: Proxy APIreg
appApi = Proxy

registryService :: ServerT APIreg App
registryService = register :<|> fsServices


myRunRedis :: (MonadReader Connection m, MonadIO m) => Redis a -> m a
myRunRedis query = do
    conn <- ask
    liftIO $ runRedis conn query


-- Register File system.
register :: Subscriber -> App RResponse
register arg = do
  let a = pack $ address arg
  let d = fromString $ (show arg)
  liftIO $ print d
  myRunRedis $ do
     rstatus <- sadd "registry" [d]
     pure $ case rstatus of
       Right a -> RResponse{status = "ok", registered = True}
       _ -> RResponse{status = "This service has not been registered", registered = False}


-- Return all the registered service memebers
fsServices :: App [Subscriber]
fsServices = do
  let services = []
  myRunRedis $ do
    list <- smembers "registry"
    pure $ case list of
      Right l -> map read $ map toString l
      _ -> []
