{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module AuthApi (AuthServiceApi,ResponseData(..),RepsoneToken(..)) where

import Servant
import Models
import Database.Persist.Postgresql
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH

data ResponseData = ResponseData
                  { status :: String
                  } deriving (Generic, ToJSON)

data RepsoneToken = RepsoneToken
                  { token :: String
                  } deriving (Generic, ToJSON)


type AuthServiceApi = "users" :> ReqBody '[JSON] User :> Post '[JSON] ResponseData
                  :<|>"verify" :> ReqBody '[JSON] User :> Post '[JSON] RepsoneToken
