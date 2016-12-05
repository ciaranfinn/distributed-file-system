{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module AuthApi () where

import Data.Aeson
import Servant


type AuthAPI = "authorise"
             :> ReqBody '[JSON] User
             :> Post '[JSON] User

api :: Proxy AuthAPI
api = Proxy
