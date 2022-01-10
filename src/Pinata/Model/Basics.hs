module Pinata.Model.Basics where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (Coercible)
import Data.Data (Typeable)
import qualified Data.Either as Either
import Data.Foldable (Foldable (fold))
import qualified Data.Morpheus.Kind as Mor
import Data.Morpheus.Types (
  GQLType,
  GQLTypeOptions (..),
 )
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

import Pinata.Model.Scalar (UUID)
import qualified Pinata.Model.Scalar as Scalar

import Text.Read (readMaybe)

data OrderingInput = OrderingInput
  { sort :: Text
  , direction :: Direction
  }
  deriving stock
    ( Generic
    , Show
    )
  deriving anyclass (GQLType)

data Direction
  = Asc
  | Desc
  | AscNullsFirst
  | DescNullsLast
  deriving stock
    ( Generic
    , Show
    )
  deriving anyclass (GQLType)

data SelectionType
  = Include
  | Exclude
  deriving stock
    ( Generic
    , Show
    )
  deriving anyclass (GQLType)

data IdSelectionInput = IdSelectionInput
  { type' :: SelectionType
  , values :: [UUID]
  }
  deriving stock
    ( Generic
    , Show
    )
  deriving anyclass (GQLType)

data SelectionInput = SelectionInput
  { type' :: SelectionType
  , values :: [Text]
  }
  deriving stock
    ( Generic
    , Show
    )
  deriving anyclass (GQLType)

data Selection m = Selection
  { type' :: m SelectionType
  , values :: m [Text]
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

data GeolocationInput = GeolocationInput
  { latitude :: Double
  , longitude :: Double
  }
  deriving stock
    ( Generic
    , Show
    )
  deriving anyclass (GQLType)

data Geolocation m = Geolocation
  { latitiude :: m Double
  , longitude :: m Double
  }

data AreaInput = AreaInput
  { location :: GeolocationInput
  , distance :: Double
  }
  deriving stock
    ( Generic
    , Show
    )
  deriving anyclass (GQLType)

data CalendarWeekday
  = Sun
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  deriving stock
    ( Generic
    , Show
    )
  deriving anyclass (GQLType)
