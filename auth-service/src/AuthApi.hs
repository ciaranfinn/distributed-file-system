{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module AuthApi (AuthServiceApi,ResponseData(..)) where

import Servant
import Models (Credentials)
import Database.Persist.Postgresql
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH

data ResponseData = ResponseData { status :: String
                             } deriving (Generic, ToJSON)

type AuthServiceApi = "verify" :> ReqBody '[JSON] Credentials :> Post '[JSON] ResponseData
