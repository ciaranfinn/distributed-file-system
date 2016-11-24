module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Database.MongoDB
import Control.Monad (when)
import System.Environment (getArgs, getProgName, lookupEnv)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

data File = File
  { name :: String
  , fileData :: String
  } deriving (Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" id "database" True

mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False

mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "FILESYSTEMDB" True

defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a

defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing ->
        return def
