module Pinata.Model.Option where

import qualified Data.Aeson as Aeson
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types (DecodeScalar, EncodeScalar, GQLType (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Pinata.Model.Basics (Direction)
import Pinata.Model.Pagination (PaginatedEdge, PaginatedEntity)
import Pinata.Model.Scalar (UUID)

data OptionConfig m = OptionConfig
  { group :: Text
  , key :: Text
  , label :: Text
  , order :: Word
  , meta :: Maybe Aeson.Value
  }
  deriving stock
    ( Generic
    )
  deriving anyclass
    ( GQLType
    )

data OptionInput = OptionInput
  { key :: Maybe Text
  , description :: Maybe Text
  , label :: Maybe Text
  , group :: Maybe Text
  }
  deriving stock
    ( Generic
    )
  deriving anyclass
    ( GQLType
    )

newtype OptionId = OptionId UUID
  deriving stock
    ( Show
    , Generic
    )
  deriving newtype
    ( DecodeScalar
    , EncodeScalar
    )

instance GQLType OptionId where
  type KIND OptionId = SCALAR

data Option m = Option
  { group :: m Text
  , key :: m Text
  , description :: m (Maybe Text)
  , active :: m (Maybe Bool)
  , label :: m Text
  }
  deriving stock
    ( Generic
    )
  deriving anyclass
    ( GQLType
    )

type PaginatedOptionEdge m = PaginatedEdge OptionId m

type PaginatedOption m =
  PaginatedEntity OptionId (Option m) m

data OrderingOptionEnum
  = OrderingOptionKey
  | OrderingOptionGroup
  | OrderingOptionOrder
  deriving stock
    ( Generic
    , Show
    )
  deriving anyclass
    ( GQLType
    )

data OrderingOptionInput = OrderingOptionInput
  { sort :: Maybe OrderingOptionEnum
  , direction :: Maybe Direction
  , args :: Maybe Aeson.Value
  }
  deriving stock
    ( Generic
    )
  deriving anyclass
    ( GQLType
    )
