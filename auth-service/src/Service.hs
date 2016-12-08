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
import           AuthApi                              (AuthServiceApi,ResponseData(..),Token(..),TokenData(..))
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


-- Program Encode Secret
secretKey :: ByteString
secretKey = "010-DIS-TOB-UTI-ONR-OCK-S01-010-"


data CipherKey k = CipherKey ByteString


newtype App a = App
    { runApp :: ReaderT ConnectionPool (ExceptT ServantErr IO) a
    } deriving ( Functor, Applicative, Monad, MonadReader ConnectionPool,
                 MonadError ServantErr, MonadIO)

app :: ConnectionPool -> Application
app cp = serve appApi (appToServer cp)

appToServer :: ConnectionPool -> Server AuthServiceApi
appToServer cp = enter (convertApp cp) authService

convertApp :: ConnectionPool -> App :~> ExceptT ServantErr IO
convertApp cp = Nat (flip runReaderT cp . runApp)

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


iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

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


-- ENCRYPTION STUFF --

encrypt :: ByteString -> ByteString -> ByteString
encrypt secret = ctrCombine ctx nullIV
  where
    ctx = cipherInitNoErr (cipherMakeKey (undefined :: AES256) secret)
    cipherInitNoErr :: BlockCipher c => CipherKey c -> c
    cipherInitNoErr (CipherKey k) = case cipherInit k of
      CryptoPassed a -> a
      CryptoFailed e -> error (show e)

    cipherMakeKey :: Cipher cipher => cipher -> ByteString -> CipherKey cipher
    cipherMakeKey _ = CipherKey

decrypt :: ByteString -> ByteString -> ByteString
decrypt = encrypt
