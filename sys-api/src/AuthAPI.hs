{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module AuthAPI (ResponseData(..),Token(..),TokenData(..)) where

import Servant
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH

data ResponseData = ResponseData
                  { status :: String,
                    valid :: Bool
                  } deriving (Generic, ToJSON)

data Token = Token
             { token :: String
             } deriving (Generic, ToJSON,FromJSON)

data TokenData = TokenData
             { email :: String,
               expiryTime :: String
             } deriving (Generic, ToJSON, FromJSON)
