{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Frequent where


import           Control.Monad.Except
import           Control.Monad.Reader                 (MonadIO, MonadReader,ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.ByteString                      (ByteString)
import           Data.ByteString.UTF8                 (toString,fromString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types                  (BlockCipher(..), Cipher(..),nullIV)
import           Crypto.Error                         (CryptoFailable(..))
import qualified Data.ByteString.Base64 as B64
import           Data.Time.Clock                      (UTCTime, getCurrentTime,addUTCTime,utctDayTime)
import           Data.Time.Format                     (defaultTimeLocale, formatTime, readTime, parseTimeOrError)
import           AuthAPI                              (ResponseData(..),Token(..),TokenData(..))
import           Data.Aeson
import           Data.Time.ISO8601



-- Return the current system time as a formatted String
systemTime :: IO String
systemTime = do
  currentTime <- getCurrentTime
  let formatted = formatISO8601 (addUTCTime 0 currentTime)
  return formatted


convertTime :: String -> Int
convertTime time = do
  let b = parseISO8601 time
  case b of
    Just c -> floor $ utctDayTime c :: Int
    Nothing -> 0


-- Used to send the time down the REST API
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"


-- ENCRYPTION STUFF --
data CipherKey k = CipherKey ByteString

-- This is the Shared system secret (Hidden Key)
secretKey :: ByteString
secretKey = "010-DIS-TOB-UTI-ONR-OCK-S01-010-"

-- This function will encrypt data
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

-- Extract the decrypted string from the encrypted (Type Swindling)
extract :: String -> String
extract encrypted = toString (decrypt secretKey (B64.decodeLenient (fromString encrypted)))

--  Get the internals of the token
getTokenData :: String -> Maybe TokenData
getTokenData encrypted = do
                      let decrypted_data = extract encrypted
                      decode (BS.pack decrypted_data) :: Maybe TokenData
