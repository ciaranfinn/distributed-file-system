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
import           Data.Bson.Generic
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant



data UpPayload = UpPayload { e_session_key :: String
                           , e_path :: String
                           , e_contents :: String
                       } deriving (Generic, FromJSON, ToBSON, FromBSON, ToJSON)


deriving instance FromBSON String
deriving instance ToBSON   String


data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON,FromBSON)



type APIfs = "store" :> ReqBody '[JSON] UpPayload :> Post '[JSON] Bool
