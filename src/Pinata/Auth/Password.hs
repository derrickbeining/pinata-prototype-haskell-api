module Pinata.Auth.Password (
  Hashed,
  hash,
  validateHashed,
) where

import qualified Crypto.BCrypt as BCrypt
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.Text as T

newtype Hashed a = Hashed {unHashed :: a}

bs2t :: BS.ByteString -> T.Text
bs2t = T.pack . BS.unpack

t2bs :: T.Text -> BS.ByteString
t2bs = BS.pack . T.unpack

hash :: T.Text -> IO (Hashed T.Text)
hash =
  fmap (Hashed . bs2t . fromJust)
    . BCrypt.hashPasswordUsingPolicy BCrypt.fastBcryptHashingPolicy
    . t2bs

validateHashed :: Hashed T.Text -> T.Text -> Bool
validateHashed (Hashed hashedPassword) originalPassword =
  BCrypt.validatePassword (t2bs hashedPassword) (t2bs originalPassword)
