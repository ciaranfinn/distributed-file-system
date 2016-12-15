{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api (Subscriber(..), API) where

import Servant
import Data.Aeson
import GHC.Generics
import Data.Aeson.TH


data Subscriber =  Subscriber
  { ip_address :: String,
    port :: Int,
    message :: String,
    service_type :: String
  } deriving(Generic,ToJSON,FromJSON)


type API = "register" :> ReqBody '[JSON] Subscriber :> Post '[JSON] Subscriber
