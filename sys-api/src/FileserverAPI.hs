{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module FileserverAPI (ResponseData(..),APIfs(..), UpPayload(..)) where

import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant



data UpPayload = UpPayload { e_session_key :: String
                           , e_path :: String
                           , e_filedata :: String
                       } deriving (Generic, FromJSON, ToJSON)


data ResponseData = ResponseData { message :: String,
                                   saved :: Bool
                                 } deriving (Generic, ToJSON, FromJSON)



type APIfs = "store" :> ReqBody '[JSON] UpPayload :> Post '[JSON] ResponseData
