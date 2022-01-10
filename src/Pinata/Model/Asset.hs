module Pinata.Model.Asset where

import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types (
  DecodeScalar,
  EncodeScalar,
  GQLType (..),
 )
import Data.Text (Text)

import GHC.Generics (Generic)

import Pinata.Model.Organization (Organization)
import Pinata.Model.Scalar (UUID)

type Program m = m ()

type Role m = m ()

newtype AssetId = AssetId UUID
  deriving stock
    ( Show
    , Generic
    )
  deriving newtype
    ( DecodeScalar
    , EncodeScalar
    )

instance GQLType AssetId where
  type KIND AssetId = SCALAR

data Asset m = Asset
  { id :: m AssetId
  , url :: m (Maybe Text)
  , name :: m (Maybe Text)
  , thumbnail :: m (Maybe Text)
  , assetType :: m (Maybe Text)
  , isPrimary :: m (Maybe Bool)
  , metadata :: m (Maybe Text)
  , public :: m (Maybe Bool)
  , extension :: m (Maybe Text)
  , label :: m (Maybe Text)
  , status :: m (Maybe Text)
  , program :: m (Maybe (Program m))
  , organization :: m (Maybe (Organization m))
  , role :: m (Maybe (Role m))
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

data UserAssetInput = UserAssetInput
  { url :: Text
  , isPrimary :: Maybe Bool
  }
