module Pinata.Model.BillingContact where

import qualified Data.Aeson as Aeson
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types (
  DecodeScalar,
  EncodeScalar,
  GQLType (..),
 )
import Data.Text (Text)

import GHC.Generics (Generic)

import Pinata.Model.Basics (Direction)
import Pinata.Model.Pagination (
  PaginatedEdge,
  PaginatedEntity,
 )
import Pinata.Model.Scalar (
  UUID,
  UtcTimestamp,
 )
import Pinata.Model.User (User)

type User m = m ()

data BillingContactInput = BillingContactInput
  { nickname :: Maybe Text
  , firstName :: Maybe Text
  , lastName :: Maybe Text
  , email :: Maybe Text
  , company :: Maybe Text
  , address1 :: Maybe Text
  , address2 :: Maybe Text
  , city :: Maybe Text
  , state :: Maybe Text
  , zip :: Maybe Text
  }
  deriving stock
    ( Generic
    , Show
    )
  deriving anyclass (GQLType)

newtype BillingContactId = BillingContactId UUID
  deriving stock
    ( Show
    , Generic
    )
  deriving newtype
    ( DecodeScalar
    , EncodeScalar
    )

instance GQLType BillingContactId where
  type KIND BillingContactId = SCALAR

data BillingContact m = BillingContact
  { id :: m BillingContactId
  , nickname :: m Text
  , firstName :: m (Maybe Text)
  , lastName :: m (Maybe Text)
  , email :: m (Maybe Text)
  , displayEmail :: m (Maybe Text)
  , company :: m (Maybe Text)
  , address1 :: m (Maybe Text)
  , address2 :: m (Maybe Text)
  , city :: m (Maybe Text)
  , state :: m (Maybe Text)
  , zip :: m (Maybe Text)
  , createdAt :: m UtcTimestamp
  , updatedAt :: m UtcTimestamp
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

type PaginatedBillingContactEdge m = PaginatedEdge BillingContactId m

type PaginatedBillingContact m =
  PaginatedEntity BillingContactId (BillingContact m) m

data OrderingBillingContactsEnum = OrderingBillingContactsNickname
  deriving stock (Generic)
  deriving anyclass (GQLType)

data OrderingBillingContactInput = OrderingBillingContactInput
  { sort :: Maybe OrderingBillingContactsEnum
  , direction :: Maybe Direction
  , args :: Maybe Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)
