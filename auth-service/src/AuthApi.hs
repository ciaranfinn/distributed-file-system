{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module AuthApi () where

import Control.Monad       (mzero)
import Data.Aeson          (FromJSON(..), ToJSON(..))
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API
import Model               (User)


type AuthAPI = "authorise" :> ReqBody '[JSON] User
             :> Post '[JSON] UserDetailed

api :: Proxy AuthAPI
api = Proxy
