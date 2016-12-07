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
import           AuthApi                              (AuthServiceApi,ResponseData(..),ResponseToken(..),Token(..))
import           Models
import           Servant
import           Crypto.PasswordStore                 (makePassword,pbkdf2,verifyPassword)
import           Data.ByteString                      (ByteString)
import           Data.ByteString.Char8                (pack,unpack)
import           Data.Time.Clock                      (UTCTime, getCurrentTime,addUTCTime)
import           Data.Time.Format                     (defaultTimeLocale, formatTime)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types                  (BlockCipher(..), Cipher(..),nullIV)
import           Crypto.Error                         (CryptoFailable(..))



data CipherKey k = CipherKey String


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
    password <- liftIO $ (makePassword (pack(userPassword user)) 20)
    newUser <- dbAction (insertBy (User email (unpack password)))
    case newUser of
        Left _ ->
            throwError err500 { errBody = "User exists" }
        Right user ->
            pure ResponseData {status = show (fromSqlKey user) }


iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

verify :: (Entity User) -> String -> Bool
verify user password = do
    let hash_pass = (userPassword $ entityVal user)
    let result = verifyPassword (pack password) (pack hash_pass)
    result

getToken :: User -> App ResponseToken
getToken user = do
    let email = userEmail user
    let password = userPassword user
    currentTime <- liftIO $ getCurrentTime
    maybeUser <- dbAction (selectFirst[UserEmail ==. email][])
    case maybeUser of
        Just user ->
            if (verify user password) then do
                let expire_time = iso8601 (addUTCTime 3600 currentTime)
                let token = BS.unpack $ encode Token {email = email, expiryTime = expire_time}
                return ResponseToken {token = token}
            else
                -- User is unauthorised
                throwError err401
        Nothing ->
            -- User has no account thus is forbidden
            throwError err500


-- Program Encode Secret
secretKey :: ByteString
secretKey = "012-456-89A-CDE-012-456-89A-CDE-"
