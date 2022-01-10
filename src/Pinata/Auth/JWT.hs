module Pinata.Auth.JWT where

import Control.Monad (guard)

import Data.Aeson.Types (Value (Bool, Number))
import qualified Data.Map as Map
import Data.Scientific (
  base10Exponent,
  coefficient,
 )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (
  posixDayLength,
  utcTimeToPOSIXSeconds,
 )
import qualified Web.JWT as Jwt
import Prelude hiding (exp)

-------------------------------------------------------------------------------
type SecretKey = Text

type Token = Text

userIDKey :: Text
userIDKey = "USERID"

-- |

-------------------------------------------------------------------------------
makeJWT :: UTCTime -> SecretKey -> Int -> Token
makeJWT currentTime secret userId =
  let cs =
        mempty -- mempty returns a default JWTClaimsSet
          { Jwt.iss = Jwt.stringOrURI "webhaskell"
          , Jwt.unregisteredClaims =
              Jwt.ClaimsMap $
                Map.fromList [(userIDKey, Number $ fromIntegral userId)]
          , Jwt.exp = Jwt.numericDate $ utcTimeToPOSIXSeconds currentTime + 30 * posixDayLength
          }
      signer = Jwt.hmacSecret secret
   in Jwt.encodeSigned signer mempty cs

-- |

-------------------------------------------------------------------------------
verifyJWT :: UTCTime -> SecretKey -> Token -> Maybe Int
verifyJWT currentTime secret token = do
  let signer = Jwt.hmacSecret secret
  unverifiedJWT <- Jwt.decode token
  verifiedJWT <- Jwt.verify signer unverifiedJWT
  expTime <- Jwt.exp . Jwt.claims $ verifiedJWT
  now <- Jwt.numericDate $ utcTimeToPOSIXSeconds currentTime
  guard (now < expTime)
  let kv = Jwt.unClaimsMap . Jwt.unregisteredClaims . Jwt.claims $ verifiedJWT
  userIDVal <- Map.lookup userIDKey kv
  case userIDVal of
    Number userID -> return . fromIntegral $ coefficient userID
    _ -> Nothing
