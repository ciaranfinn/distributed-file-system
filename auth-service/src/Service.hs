{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Service where

import           Database.Persist.Postgresql          (Entity(..),
                                                       ConnectionPool,
                                                       selectList,
                                                       selectFirst,
                                                       fromSqlKey,
                                                       insertBy,
                                                       insert,
                                                       (==.))
import           Control.Monad.Except
import           Control.Monad.Reader                 (MonadIO, MonadReader,ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Models
import           Servant
import           Crypto.PasswordStore                 (makePassword,pbkdf2,verifyPassword)
import           Data.ByteString                      (ByteString)
import           Data.ByteString.UTF8                 (toString,fromString)
import           Data.Time.Clock                      (UTCTime, getCurrentTime,addUTCTime)
import           Data.Time.Format                     (defaultTimeLocale, formatTime)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types                  (BlockCipher(..), Cipher(..),nullIV)
import           Crypto.Error                         (CryptoFailable(..))
import qualified Data.ByteString.Base64 as B64
import           Frequent
import           AuthAPI                              (ResponseData(..),Token(..),TokenData(..))


type AuthServiceApi = "create" :> ReqBody '[JSON] User :> Post '[JSON] ResponseData
                  :<|>"login" :> ReqBody '[JSON] User :> Post '[JSON] Token

type App = ReaderT ConnectionPool Handler

app :: ConnectionPool -> Application
app cp = serve appApi (appToServer cp)

appToServer :: ConnectionPool -> Server AuthServiceApi
appToServer cp = enter (runReaderTNat cp) authService

appApi :: Proxy AuthServiceApi
appApi = Proxy

authService :: ServerT AuthServiceApi App
authService = createUser :<|> getToken


-- Service Actions --
createUser :: User -> App ResponseData
createUser user = do
    let email = userEmail user
    password <- liftIO $ (makePassword (fromString (userPassword user)) 20)
    newUser <- dbAction (insertBy (User email (toString password)))
    case newUser of
        Left _ ->
            throwError err500 { errBody = "User exists" }
        Right user ->
            pure ResponseData {status = show (fromSqlKey user), valid = True}


verify :: (Entity User) -> String -> Bool
verify user password = do
    let hash_pass = (userPassword $ entityVal user)
    let result = verifyPassword (fromString password) (fromString hash_pass)
    result

getToken :: User -> App Token
getToken user = do
    let email = userEmail user
    let password = userPassword user
    currentTime <- liftIO $ getCurrentTime
    maybeUser <- dbAction (selectFirst[UserEmail ==. email][])
    case maybeUser of
        Just user ->
            if (verify user password) then do
                let expire_time = iso8601 (addUTCTime 3600 currentTime)
                let token = BS.unpack $ encode TokenData {email = email, expiryTime = expire_time}
                let encrypted_token = toString $ B64.encode (encrypt secretKey (fromString token))
                return Token {token = encrypted_token}
                -- Broadcast token to other registered services
            else
                -- User is unauthorised
                throwError err401
        Nothing ->
            -- User has no account therfore is forbidden
            throwError err500
