{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module RegistryAPI (Subscriber(..),RResponse(..), APIreg) where

import Servant
import Data.Aeson
import GHC.Generics
import Data.Aeson.TH

-- The file service pings the registry to let it know its alive
data Subscriber =  Subscriber
  { address :: String,
    port :: Int,
    message :: String,
    service_type :: String
  } deriving(Generic,Show,ToJSON,FromJSON,Read)


data RResponse = RResponse
  {
    status :: String,
    registered :: Bool
  } deriving(Generic, ToJSON, FromJSON)


type APIreg = "register" :> ReqBody '[JSON] Subscriber :> Post '[JSON] RResponse
        :<|> "registered" :> Get '[JSON] [Subscriber]
