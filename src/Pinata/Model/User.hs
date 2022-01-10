module Pinata.Model.User (
  User (..),
  UserRow (..),
  UserPKey,
  UserPKey',
  UserPKeyReadField,
  UserPKeyWriteField,
  UserUUID,
  UserUUIDReadField,
  UserUUIDWriteField,
  pUserPKey,
  pUserUUID,
  userResolver,
  userByUUIDResolver,
  userByPKeyResolver,
) where

import qualified Data.Morpheus as GQL
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types (
  Arg (argValue),
  DecodeScalar,
  EncodeScalar,
  GQLType (..),
 )
import qualified Data.Morpheus.Types as GQL

import Data.Text (Text)

import GHC.Generics (Generic)

import Data.Data (Typeable)
import Data.Profunctor.Product.Default (Default (..))
import qualified Data.Profunctor.Product.TH as PPTH
import qualified Data.UUID as UUID
import Opaleye (fromPGSFromField, (.===))
import qualified Opaleye as DB
import qualified Pinata.DB as DB
import Pinata.Graphql (GraphQL, Value, runSelectMaybeOne, runSelectOne)
import qualified Pinata.Model.Scalar as Scalar

--
-- DATABASE MODEL
--

newtype UserPKey' a = UserPKey
  { unUserPKey :: a
  }
  deriving stock
    ( Eq
    , Generic
    )
  deriving newtype
    ( Show
    )

$(PPTH.makeAdaptorAndInstanceInferrable "pUserPKey" ''UserPKey')

type UserPKey = UserPKey' Int

type UserPKeyReadField =
  UserPKey' (DB.Field DB.PGInt4)

type UserPKeyWriteField =
  UserPKey' () -- Disallow writing to the pkey column

instance DB.DefaultFromField DB.SqlInt4 (Pinata.Model.User.UserPKey' Int) where
  defaultFromField = DB.unsafeFromField UserPKey (DB.defaultFromField @DB.PGInt4)

newtype UserUUID' a = UserUUID
  { unUserUUID :: a
  }
  deriving stock
    ( Generic
    , Show
    , Eq
    )
  deriving newtype
    ( DecodeScalar
    , EncodeScalar
    )

$(PPTH.makeAdaptorAndInstanceInferrable "pUserUUID" ''UserUUID')

instance Typeable a => GQLType (UserUUID' a) where
  type KIND (UserUUID' a) = SCALAR

type UserUUID = UserUUID' Scalar.UUID

type UserUUIDReadField =
  UserUUID' (DB.Field DB.PGUuid)

-- | use Maybe because we don't need to specify uuid when inserting
type UserUUIDWriteField =
  UserUUID' (Maybe (DB.Field DB.PGUuid))

--
-- Table Definition
--

data
  UserRow'
    pkey
    uuid = UserRow
  { pkey' :: pkey
  , uuid' :: uuid
  }

$(PPTH.makeAdaptorAndInstanceInferrable "pUserRow" ''UserRow')

type UserRow =
  DB.TimestampedRow
    ( UserRow'
        UserPKey -- pkey
        UserUUID -- uuid
    )

type UserWriteField =
  DB.TimestampedWriteField
    ( UserRow'
        UserPKeyWriteField
        UserUUIDWriteField
    )

type UserReadField =
  DB.TimestampedReadField
    ( UserRow'
        UserPKeyReadField
        UserUUIDReadField
    )

table :: DB.Table UserWriteField UserReadField
table =
  DB.table "organizations" . DB.pTimestampedTable . DB.withTimestampFields $
    pUserRow rowDef
  where
    rowDef =
      UserRow
        { pkey' = pUserPKey . UserPKey $ DB.readOnlyTableField "id"
        , uuid' = pUserUUID . UserUUID $ DB.tableField "uuid"
        }

--
-- DATABASE QUERIES
--

select :: DB.Select UserReadField
select = DB.selectTable table

findByUUID :: UserUUID -> DB.Select UserReadField
findByUUID userUUID = do
  user <- select
  DB.where_ $ uuid' (DB.record user) .=== DB.toFields userUUID
  return user

findByPKey :: UserPKey -> DB.Select UserReadField
findByPKey pkey = do
  user <- select
  DB.where_ $ pkey' (DB.record user) .=== DB.toFields pkey
  return user

--
-- GRAPHQL MODEL
--

data User m = User
  { uuid :: m UserUUID
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

--
-- # Resolvers
--
userResolver ::
  (Monad m, GraphQL o) =>
  UserRow ->
  Value o (User m)
userResolver org =
  let UserRow{..} = DB.record org
   in pure
        User
          { uuid = pure uuid'
          }

userByUUIDResolver ::
  (GraphQL o, Monad m) =>
  Arg "uuid" UserUUID ->
  Value o (User m)
userByUUIDResolver uuidArg = do
  let q = findByUUID . argValue $ uuidArg
  it <- runSelectOne q "Invalid org uuid"
  userResolver it

userByPKeyResolver ::
  (GraphQL o, Monad m) =>
  Arg "userPKey" UserPKey ->
  Value o (Maybe (User m))
userByPKeyResolver pkeyArg = do
  let userPKey = argValue pkeyArg
      q = findByPKey userPKey
      pkeyStr = show userPKey
  mUser <- runSelectMaybeOne q
  maybe (pure Nothing) (fmap Just . userResolver) mUser
