{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Service () where

import Api
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Servant
