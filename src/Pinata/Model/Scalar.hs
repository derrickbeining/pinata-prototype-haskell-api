module Pinata.Model.Scalar (
  Date (..),
  UUID (..),
  uuidToText,
  Json (..),
  LocalTimestamp (..),
  PosixTimestamp (..),
  UtcTimestamp (..),
  ZonedTimestamp (..),
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (Coercible)
import Data.Data (Typeable)
import qualified Data.Either as Either
import Data.Fixed (Pico)
import Data.Foldable (Foldable (fold))
import qualified Data.Morpheus.Kind as Mor
import Data.Morpheus.Types (GQLTypeOptions (..))
import qualified Data.Morpheus.Types as Mor
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Time as Time
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Clock.POSIX as Time
import Data.Time.Format.ISO8601 (
  ISO8601,
  iso8601ParseM,
  iso8601Show,
 )
import qualified Data.Time.Format.ISO8601 as ISO
import qualified Data.UUID as UUID

import GHC.Generics (Generic)

import Data.Profunctor.Product.Default (Default (..))
import qualified Data.Profunctor.Product.TH as PPTH
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as DB
import qualified Opaleye as DB
import Text.Read (readMaybe)

-- # UUID

newtype UUID = UUID
  { unUUID :: UUID.UUID
  }
  deriving
    ( Generic
    , Show
    , Typeable
    )

instance Default DB.ToFields UUID (DB.Column DB.PGUuid) where
  def = DB.toToFields (DB.sqlUUID . unUUID)

instance DB.DefaultFromField DB.PGUuid UUID where
  defaultFromField = DB.unsafeFromField UUID (DB.defaultFromField @DB.PGUuid)

uuidToText :: UUID -> Text
uuidToText (UUID uuid) = UUID.toText uuid

instance Mor.DecodeScalar UUID where
  decodeScalar scalar = case scalar of
    Mor.String txt ->
      maybe
        (Left $ txt <> " cannot be decoded to a scalar UUID value.")
        Right
        (UUID <$> UUID.fromText txt)
    val ->
      Left . decodeUtf8 . BSL.toStrict $
        Aeson.encodePretty (Aeson.toJSON val)
          <> " cannot be decoded to a scalar UUID value."

instance Mor.EncodeScalar UUID where
  encodeScalar (UUID uuid) =
    Mor.String (UUID.toText uuid)

instance Mor.GQLType UUID where
  type KIND UUID = Mor.SCALAR

  description _ = Just "A UUID primary key, which serializes to a UUID string"

-- # Json
newtype Json = Json Aeson.Value
  deriving
    ( Generic
    , Show
    )

instance Mor.DecodeScalar Json where
  decodeScalar val = Right $ Json $ Aeson.toJSON val

instance Mor.EncodeScalar Json where
  encodeScalar (Json json) = Mor.Value json

instance Mor.GQLType Json where
  type KIND Json = Mor.SCALAR

  description _ =
    Just
      "The `JSON` scalar type represents JSON values as specified by [ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf)."

--
-- # Date
--

newtype Date = Date Time.Day
  deriving stock
    ( Show
    , Generic
    )
  deriving newtype (ISO8601)

instance Mor.DecodeScalar Date where
  decodeScalar scalar = case scalar of
    Mor.String txt ->
      iso8601ParseM
        (Text.unpack txt)
    val ->
      Left $
        decodeUtf8 $
          BSL.toStrict $
            Aeson.encodePretty (Aeson.toJSON val)
              <> " cannot be decoded to a Date scalar value."

instance Mor.EncodeScalar Date where
  encodeScalar date = Mor.String (Text.pack $ iso8601Show date)

instance Mor.GQLType Date where
  type KIND Date = Mor.SCALAR

  description _ =
    Just "A calendar date that serializes to ISO-8601 extended format (YYYY-MM-DD)."

--
-- # ZonedTimeStamp
--

newtype ZonedTimestamp = ZonedTimestamp Time.ZonedTime
  deriving stock
    ( Show
    , Generic
    )
  deriving newtype (ISO8601)

instance Mor.DecodeScalar ZonedTimestamp where
  decodeScalar scalar = case scalar of
    Mor.String txt ->
      iso8601ParseM
        (Text.unpack txt)
    val ->
      Left $
        decodeUtf8 $
          BSL.toStrict $
            Aeson.encodePretty (Aeson.toJSON val)
              <> " cannot be decoded to a ZonedTimestamp scalar value."

instance Mor.EncodeScalar ZonedTimestamp where
  encodeScalar zonedTimestamp =
    Mor.String (Text.pack $ iso8601Show zonedTimestamp)

instance Mor.GQLType ZonedTimestamp where
  type KIND ZonedTimestamp = Mor.SCALAR

  description _ =
    Just
      "A local time together with a time zone which serializes to ISO-8601 extended format (yyyy-mm-ddThh:mm:ss[.sss]±hh:mm)"

--
-- # LocalTimestamp
--

newtype LocalTimestamp = LocalTimestamp Time.LocalTime
  deriving stock
    ( Show
    , Generic
    )
  deriving newtype (ISO8601)

instance Mor.DecodeScalar LocalTimestamp where
  decodeScalar scalar = case scalar of
    Mor.String txt ->
      iso8601ParseM
        (Text.unpack txt)
    val ->
      Left $
        decodeUtf8 $
          BSL.toStrict $
            Aeson.encodePretty (Aeson.toJSON val)
              <> " cannot be decoded to a LocalTimestamp scalar value."

instance Mor.EncodeScalar LocalTimestamp where
  encodeScalar zonedTimestamp =
    Mor.String (Text.pack $ iso8601Show zonedTimestamp)

instance Mor.GQLType LocalTimestamp where
  type KIND LocalTimestamp = Mor.SCALAR

  description _ =
    Just
      "A local time which serializes to ISO-8601 extended format (yyyy-mm-ddThh:mm:ss[.sss])"

--
-- # UtcTimestamp
--

newtype UtcTimestamp = UtcTimestamp {unUtcTimestamp :: Time.UTCTime}
  deriving stock
    ( Show
    , Generic
    )
  deriving newtype (ISO8601)

instance Default DB.ToFields UtcTimestamp (DB.Column DB.PGTimestamptz) where
  def = DB.toToFields (DB.pgUTCTime . unUtcTimestamp)

instance DB.DefaultFromField DB.PGTimestamptz UtcTimestamp where
  defaultFromField = DB.unsafeFromField UtcTimestamp (DB.defaultFromField @DB.PGTimestamptz)

instance Mor.DecodeScalar UtcTimestamp where
  decodeScalar scalar = case scalar of
    Mor.String txt ->
      iso8601ParseM
        (Text.unpack txt)
    val ->
      Left $
        decodeUtf8 $
          BSL.toStrict $
            Aeson.encodePretty (Aeson.toJSON val)
              <> " cannot be decoded to a UtcTimestamp scalar value."

instance Mor.EncodeScalar UtcTimestamp where
  encodeScalar utcTimestamp =
    Mor.String (Text.pack $ iso8601Show utcTimestamp)

instance Mor.GQLType UtcTimestamp where
  type KIND UtcTimestamp = Mor.SCALAR

  description _ =
    Just
      "A Modified Julian Day number and a time offset from midnight, which serializes to ISO-8601 extended format (yyyy-mm-ddThh:mm:ss[.sss]Z)"

--
-- # PosixTimestamp
--

newtype PosixTimestamp = PosixTimestamp Time.POSIXTime
  deriving stock
    ( Show
    , Generic
    )

instance Mor.DecodeScalar PosixTimestamp where
  decodeScalar scalar = case scalar of
    Mor.String txt -> maybe
      (Left $ txt <> " cannot be decoded to a PosixTimestamp scalar value.")
      Right
      $ do
        millis <- (readMaybe (Text.unpack txt) :: Maybe Word)
        let fractionalSeconds = realToFrac millis / 10000 :: Pico
        pure $ PosixTimestamp $ Time.secondsToNominalDiffTime fractionalSeconds
    val ->
      Left $
        decodeUtf8 $
          BSL.toStrict $
            Aeson.encodePretty (Aeson.toJSON val)
              <> " cannot be decoded to a PosixTimestamp scalar value."

instance Mor.EncodeScalar PosixTimestamp where
  encodeScalar (PosixTimestamp posix) = Mor.Int millis
    where
      millis = truncate $ Time.nominalDiffTimeToSeconds posix * 1000

instance Mor.GQLType PosixTimestamp where
  type KIND PosixTimestamp = Mor.SCALAR

  description _ =
    Just
      "A Modified Julian Day number and a time offset from midnight, which serializes to a Unix-epoch timestamp in milliseconds"
