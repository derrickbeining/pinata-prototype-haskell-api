module Pinata.Model.User (
  User (..),
  UserRow (..),
  UserRow' (..),
  PKey,
  PKey' (PKey, unPKey),
  PKeyReadField,
  PKeyWriteField,
  ReadField,
  Uuid,
  UuidReadField,
  UuidWriteField,
  WriteField,
  findByPKey,
  findByUUID,
  pPKey,
  pPKeyTableField,
  pUuid,
  pUuidTableField,
  resolve,
  resolveByPKey,
  resolveMaybeByPKey,
  resolverByUuid,
  select,
) where

import Data.Data (Typeable)
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

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import Data.Profunctor.Product.Default (Default (..))
import qualified Data.Profunctor.Product.TH as PPTH
import qualified Data.UUID as UUID
import Opaleye (fromPGSFromField, (.===))
import qualified Opaleye as DB
import qualified Pinata.DB as DB
import qualified Pinata.Graphql as GQL
import qualified Pinata.Model.Scalar as Scalar

--
-- DATABASE MODEL
--

-- | DO NOT EXPOSE CONSTRUCTOR
newtype PKey' a = PKey
  { unPKey :: a
  }
  deriving stock
    ( Eq
    , Functor
    , Generic
    )
  deriving newtype
    ( Show
    , DecodeScalar
    , EncodeScalar
    )

$(PPTH.makeAdaptorAndInstanceInferrable "pPKey" ''PKey')

-- | USE THIS FOR FKEYS; DON'T EXPOSE PKey CONSTRUCTOR
pPKeyTableField :: DB.TableFields a b -> DB.TableFields (PKey' a) (PKey' b)
pPKeyTableField = pPKey . PKey

type PKey = PKey' Int

type PKeyReadField =
  PKey' (DB.Field DB.PGInt4)

type PKeyWriteField =
  PKey' () -- Disallow writing to the pkey column

instance Typeable a => GQLType (PKey' a) where
  type KIND (PKey' a) = SCALAR
  typeOptions _ opts =
    opts
      { GQL.typeNameModifier = \isInput name ->
          "UserPKey"
      }

instance DB.DefaultFromField DB.SqlInt4 (Pinata.Model.User.PKey' Int) where
  defaultFromField = DB.unsafeFromField PKey (DB.defaultFromField @DB.PGInt4)

-- | DO NOT EXPOSE CONSTRUCTOR
newtype Uuid' a = Uuid
  { unUuid :: a
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

$(PPTH.makeAdaptorAndInstanceInferrable "pUuid" ''Uuid')

-- | USE THIS FOR FKEYS; DON'T EXPOSE PKey CONSTRUCTOR
pUuidTableField :: PP.ProductProfunctor p => p a b -> p (Uuid' a) (Uuid' b)
pUuidTableField = pUuid . Uuid

type Uuid = Uuid' Scalar.UUID

type UuidReadField =
  Uuid' (DB.Field DB.PGUuid)

-- | use Maybe because we don't need to specify uuid when inserting
type UuidWriteField =
  Uuid' (Maybe (DB.Field DB.PGUuid))

instance Typeable a => GQLType (Uuid' a) where
  type KIND (Uuid' a) = SCALAR
  typeOptions _ opts =
    opts
      { GQL.typeNameModifier = \isInput name ->
          "UserUuid"
      }

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
        PKey -- pkey
        Uuid -- uuid
    )

type WriteField =
  DB.TimestampedWriteField
    ( UserRow'
        PKeyWriteField
        UuidWriteField
    )

type ReadField =
  DB.TimestampedReadField
    ( UserRow'
        PKeyReadField
        UuidReadField
    )

table :: DB.Table WriteField ReadField
table =
  DB.table "users" . DB.pTimestampedRow . DB.withTimestampFields $
    pUserRow rowDef
  where
    rowDef =
      UserRow
        { pkey' = pPKey . PKey $ DB.readOnlyTableField "id"
        , uuid' = pUuid . Uuid $ DB.tableField "uuid"
        }

--
-- DATABASE QUERIES
--

select :: DB.Select ReadField
select = DB.selectTable table

findByUUID :: Uuid -> DB.Select ReadField
findByUUID uuid = do
  user <- select
  DB.where_ $ uuid' (DB.record user) .=== DB.toFields uuid
  return user

findByPKey :: PKey -> DB.Select ReadField
findByPKey pkey = do
  user <- select
  DB.where_ $ pkey' (DB.record user) .=== DB.toFields pkey
  return user

--
-- GRAPHQL MODEL
--

data User m = User
  { uuid :: m Uuid
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

--
-- # Resolvers
--
resolve ::
  (GQL.MonadGraphQL o m) =>
  UserRow ->
  GQL.Value o m User
resolve user =
  let UserRow{..} = DB.record user
   in pure
        User
          { uuid = pure uuid'
          }

resolverByUuid ::
  (GQL.MonadGraphQL o m) =>
  Arg "uuid" Uuid ->
  GQL.Value o m User
resolverByUuid uuidArg = do
  let q = findByUUID . argValue $ uuidArg
  it <- GQL.runSelectOne q "Invalid user uuid"
  resolve it

resolveByPKey ::
  (GQL.MonadGraphQL o m) =>
  Arg "pkey" PKey ->
  GQL.Value o m User
resolveByPKey pkeyArg = do
  let userPKey = argValue pkeyArg
      q = findByPKey userPKey
  it <- GQL.runSelectOne q "Invalid user pkey"
  resolve it

resolveMaybeByPKey ::
  (GQL.MonadGraphQL o m) =>
  Arg "pkey" PKey ->
  GQL.Composed o m Maybe User
resolveMaybeByPKey pkeyArg = do
  let userPKey = argValue pkeyArg
      q = findByPKey userPKey
      pkeyStr = show userPKey
  mUser <- GQL.runSelectMaybeOne q
  maybe (pure Nothing) (fmap Just . resolve) mUser
