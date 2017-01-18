
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

module FsAPi (ResponseData(..),API(..),Message(..)) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant



data Message = Message { filename :: String
                       , filedata :: String
                       } deriving (Generic, FromJSON, ToBSON, FromBSON, ToJSON)

deriving instance FromBSON String
deriving instance ToBSON   String


data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON,FromBSON)



type API = "upload" :> ReqBody '[JSON] Message :> Post '[JSON] Bool
        :<|> "searchMessage" :> QueryParam "name" String :> Get '[JSON] [Message]
