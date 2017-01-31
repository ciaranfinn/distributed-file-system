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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


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
import           Network.HTTP.Client          (Manager,defaultManagerSettings,
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
import           AuthAPI                      (TokenData(..))
import           System.Directory             (doesFileExist)
import           Database.MongoDB
import           System.Environment           (getProgName)
import           MongoDb                      (drainCursor, runMongo, logLevel)
import           Frequent
import qualified Data.ByteString.Base64 as B64
import           Data.ByteString.UTF8         (toString,fromString)
import           MongoDb
import           DirectoryAPI
import           RegistryAPI
import           System.Random                       (randomRIO)


startApp :: IO ()
startApp  = withLogging $ \ aplogger -> do
  warnLog "Starting Directory Service"
  forkIO $ taskScheduler 5
  let settings = setPort 9002 $ setLogger aplogger defaultSettings
  runSettings settings app


taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Task scheduler operating."
  threadDelay $ delay * 1000000
  taskScheduler delay


type App = Handler

app :: Application
app = serve dirAPI appToServer

appToServer :: Server APIdir
appToServer =  getServer


--  Make a request to the registry to obtain a file server node
findServer :: IO (Maybe Subscriber)
findServer = do
        response <- (SC.runClientM getRegistered =<< env)
        case response of
                Right subscribers -> do
                    server <- pickRandomServer subscribers
                    pure $ Just server

                Left e -> pure Nothing

        where env = do
               manager <- newManager defaultManagerSettings
               let (host,port) = registry_service
               return (SC.ClientEnv manager (SC.BaseUrl SC.Http host port ""))



-- Not the gretest selection means, yet the directory will return a node to the client
pickRandomServer :: [Subscriber] -> IO Subscriber
pickRandomServer xs = randomRIO (0, length xs - 1) >>= return . (xs !!)




-- Return a registered node to the client
getServer :: FSRequest -> App FSInfo
getServer session@(FSRequest my_session_key) = liftIO $ do
  let token = getTokenData my_session_key
  case token of
    Just t -> do
      systemt <- systemTime
      let expiry = convertTime $ expiryTime t
      let sys = convertTime systemt

      if (sys < expiry)
        then do
          node <- findServer
          case node of
            Just info -> do
              let encrypted_data = toString $ B64.encode (encrypt secretKey (fromString (show info)))
              return FSInfo{ node = encrypted_data }
            Nothing -> return FSInfo{ node ="" }
        else
          return FSInfo{ node ="" }

    Nothing -> return FSInfo{ node ="" }



  -- locking
  -- The client asks the Directory fto write a file.
  -- The directory checks for the if the file is locked by another server.
  -- if the file is locked the server will return wanrn the user.
  -- The client can retry and obtain the lock. Once the client has been granted a lock
  -- it should be given permission to write



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
