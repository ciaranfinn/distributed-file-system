
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
import           Control.Monad.Except         (runExceptT)
import           Control.Monad.Reader         (MonadIO, MonadReader,ReaderT, runReaderT, asks)
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
import           FileserverAPI
import           AuthAPI                      (TokenData(..))
import           System.Directory             (doesFileExist)
import           Database.MongoDB
import           System.Environment           (getProgName)
import           MongoDb                      (drainCursor, runMongo, logLevel)
import           Frequent
import qualified Data.ByteString.Base64 as B64
import           Data.ByteString.UTF8         (toString,fromString)
import           MongoDb
import           RegistryAPI
import           System.Random                       (randomRIO)


startApp :: Int -> IO ()
startApp runPort = withLogging $ \ aplogger -> do
  warnLog "Starting File Server Service"
  forkIO $ taskScheduler 5
  let settings = setPort runPort $ setLogger aplogger defaultSettings
  runSettings settings (app runPort)


taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Task scheduler operating."
  threadDelay $ delay * 1000000
  taskScheduler delay


runReplicator :: Int -> UpPayload -> IO ()
runReplicator port file = do
  print "Replicate file"
  manager <- newManager defaultManagerSettings
  liftIO $ forkIO $ replicateFile port file manager
  pure ()


-- Replicate file to another file-server node in the network
replicateFile :: Int -> UpPayload -> Manager -> IO ()
replicateFile runPort file manager = do
  print "Discovering FS Services"
  node <- findServer runPort
  case node of
    Just fs -> do
      response <- (SC.runClientM (upload file) (SC.ClientEnv manager (SC.BaseUrl SC.Http (address fs) (port fs) "")))
      print "replicated to new node"

    Nothing -> print "Registry is offline, no replication occured"


-- Make a request to the registry to obtain the list of registered file servers
findServer :: Int -> IO (Maybe Subscriber)
findServer runPort = do
        response <- (SC.runClientM getRegistered =<< env)
        case response of
                Right subscribers -> do
                    -- If server list is empty no replication can occur
                    if (null subscribers)
                      then do
                        pure Nothing
                      else do
                        server <- pickRandomServer runPort subscribers
                        pure $ Just server

                Left e -> pure Nothing

        where env = do
               manager <- newManager defaultManagerSettings
               let (host,port) = registry_service
               return (SC.ClientEnv manager (SC.BaseUrl SC.Http host port ""))



-- Choose a server yet do not pick the current one as we will loop (replication to ourself)
-- we can identify ourself via our port. Each Fileserver will be spun up on a different node.
pickRandomServer :: Int -> [Subscriber] -> IO Subscriber
pickRandomServer runPort xs = do
  let availableNodes = filter ((runPort /=) . (port)) xs
  liftIO $ randomRIO (0, length availableNodes - 1) >>= return . (availableNodes !!)

-- We will need to use this to eliminate our node when taking certian actions.
data Important = Important {sPort :: Int}

type App = ReaderT Important Handler

app :: Int -> Application
app port = serve fsAPI (appToServer (Important $ port))

appToServer :: Important -> Server APIfs
appToServer p = enter (runReaderTNat p) fsService

fsService :: ServerT APIfs App
fsService = store :<|> download

    -- Store file payload in the bucket within the service
store :: UpPayload -> App ResponseData
store file@(UpPayload e_session_key path e_filedata) = do
  runPort <- asks sPort
  liftIO $ do
    let token = getTokenData e_session_key
    case token of
      Just t -> do
                systemt <- systemTime
                let expiry = convertTime $ expiryTime t
                let sys = convertTime systemt

                -- If the token is out of date, fling the user out of the system
                if (sys < expiry)
                  then do
                    doc <- runMongo $ do
                        docs <- find (select ["filename" =: path] "FILE_STORE") >>= drainCursor
                        return docs

                    -- if we don't see that the file is saved we will add it
                    if (null doc)
                      then do
                        runMongo $ upsert (select ["filename" =: path] "FILE_STORE") ["filename" =: path]
                        writeFile ("bucket/" ++ path) (extract e_filedata)
                        runReplicator runPort file
                        return ResponseData{ saved = True}
                      else
                        return ResponseData{ saved = True}
                  else
                    return ResponseData{ saved = False}

      Nothing -> return ResponseData{ saved = False}


--  Supply a path and get the encrypted file in the response payload
download :: DownRequest -> App DownPayload
download msg@(DownRequest filepath session_key ) = liftIO $ do
  let token = getTokenData session_key
  case token of
    Just t -> do
              systemt <- systemTime
              let expiry = convertTime $ expiryTime t
              let sys = convertTime systemt

              if (sys < expiry)
                then do
                  present <- liftIO $ doesFileExist ("bucket/" ++ filepath)
                  if present
                    then do
                      file <- liftIO $ readFile ("bucket/" ++ filepath)
                      let encrypted_file = toString $ B64.encode (encrypt secretKey (fromString file))
                      liftIO $ return DownPayload{ filename = filepath, e_data = encrypted_file}
                    else
                      return DownPayload{ filename = "File doesn't exist", e_data = ""}
                else
                  return DownPayload{ filename = "", e_data = ""}

    -- unauthorised
    Nothing -> return DownPayload{ filename = "", e_data = ""}


-- global logging functions
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
