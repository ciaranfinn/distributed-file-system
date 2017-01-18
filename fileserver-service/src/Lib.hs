
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib (startApp) where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           FsAPi                        (API, ResponseData, Message(..))
import           Database.MongoDB
import           System.Environment           (getProgName)
import           MongoDb                      (drainCursor, runMongo, logLevel)


startApp :: IO ()
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting File Server Service"

  forkIO $ taskScheduler 5
  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app


taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Task scheduler operating."

  threadDelay $ delay * 1000000
  taskScheduler delay


app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = upload :<|> searchMessage
  where


    upload :: Message -> Handler Bool
    upload msg@(Message key _) = liftIO $ do
      warnLog $ "Storing message under key " ++ key ++ "."
      runMongo $ upsert (select ["name" =: key] "MESSAGE_RECORD") $ toBSON msg
      return True

    searchMessage :: Maybe String -> Handler [Message]
    searchMessage (Just key) = liftIO $ do
      warnLog $ "Searching for value for key: " ++ key

      runMongo $ do
        docs <- find (select ["name" =: key] "MESSAGE_RECORD") >>= drainCursor
        return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Message) docs

    searchMessage Nothing = liftIO $ do
      warnLog $ "No key for searching."
      return $ ([] :: [Message])



iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global login functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger
