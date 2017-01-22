{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE StandaloneDeriving #-}


module RegistryAPI (APIreg,Subscriber(..),RResponse(..), regAPI) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Servant
import           Data.Proxy
import           Servant.API
import           Servant.Client

-- The file service pings the registry to let it know its alive
data Subscriber =  Subscriber
  { address :: String,
    port :: Int,
    message :: String,
    service_type :: String
  } deriving(Eq, Show, Generic, ToJSON, FromJSON, Read)


data RResponse = RResponse
  { status :: String,
    registered :: Bool
  } deriving(Eq, Show, Generic, ToJSON, FromJSON)


type APIreg = "register" :> ReqBody '[JSON] Subscriber :> Post '[JSON] RResponse
        :<|> "registered" :> Get '[JSON] [Subscriber]


regAPI :: Proxy APIreg
regAPI = Proxy

register :: Subscriber -> ClientM RResponse
getRegistered :: ClientM [Subscriber]

(register :<|> getRegistered) = client regAPI
