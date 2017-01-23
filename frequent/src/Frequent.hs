{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Frequent where

import           Data.ByteString                      (ByteString)
import           Data.ByteString.UTF8                 (toString,fromString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types                  (BlockCipher(..), Cipher(..),nullIV)
import           Crypto.Error                         (CryptoFailable(..))
import qualified Data.ByteString.Base64 as B64
import           Data.Time.Clock                      (UTCTime, getCurrentTime,addUTCTime)
import           Data.Time.Format                     (defaultTimeLocale, formatTime)
import           AuthAPI                              (ResponseData(..),Token(..),TokenData(..))
import           Data.Aeson

-- ENCRYPTION STUFF --
data CipherKey k = CipherKey ByteString

secretKey :: ByteString
secretKey = "010-DIS-TOB-UTI-ONR-OCK-S01-010-"

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

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

validateToken :: Token -> IO String
validateToken t = do
    let value = token t
    let decrypted_data = toString (decrypt secretKey (B64.decodeLenient (fromString value)))
    let tokenData = decode (BS.pack decrypted_data) :: Maybe TokenData
    case tokenData of
        Just internal -> do
            pure (expiryTime internal)
        Nothing ->
            pure ""
