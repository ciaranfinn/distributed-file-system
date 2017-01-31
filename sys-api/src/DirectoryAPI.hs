{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE StandaloneDeriving #-}


module DirectoryAPI where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Servant
import           Data.Proxy
import           Servant.API
import           Servant.Client


data FSRequest = FSRequest
  { my_session_key :: String
  } deriving(Eq, Show, Generic, ToJSON, FromJSON)

data FSInfo = FSInfo
  { node :: String
  } deriving(Eq, Show, Generic, ToJSON, FromJSON)


type APIdir= "getFSNode" :> ReqBody '[JSON] FSRequest :> Post '[JSON] FSInfo


dirAPI :: Proxy APIdir
dirAPI = Proxy

getFSNode :: FSRequest -> ClientM FSInfo

(getFSNode) = client dirAPI
