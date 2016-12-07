{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module AuthApi (AuthServiceApi,ResponseData(..),ResponseToken(..),Token(..)) where

import Servant
import Models
import Database.Persist.Postgresql
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH

data ResponseData = ResponseData
                  { status :: String
                  } deriving (Generic, ToJSON)

data ResponseToken = ResponseToken
                  { token :: String
                  } deriving (Generic, ToJSON)

data Token = Token
             { email :: String,
               expiryTime :: String
             } deriving (Generic, ToJSON, FromJSON)


type AuthServiceApi = "users" :> ReqBody '[JSON] User :> Post '[JSON] ResponseData
                  :<|>"verify" :> ReqBody '[JSON] User :> Post '[JSON] ResponseToken
