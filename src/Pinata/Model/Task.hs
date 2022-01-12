module Pinata.Model.Task (
  Task (..),
  TaskRow (..),
  PKey,
  PKey',
  PKeyReadField,
  PKeyWriteField,
  Uuid,
  UuidReadField,
  UuidWriteField,
  pPKey,
  pUuid,
  resolve,
  resolveAll,
  resolveByUuid,
  resolveByPKey,
  resolveMaybeByPKey,
) where

import qualified Data.Morpheus as GQL
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types (
  Arg (Arg, argValue),
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
import qualified Pinata.Graphql as GQL
import qualified Pinata.Model.Scalar as Scalar
import Pinata.Model.User (User)
import qualified Pinata.Model.User as User

--
-- DATABASE MODEL
--

newtype PKey' a = PKey
  { unPKey :: a
  }
  deriving stock
    ( Eq
    , Generic
    )
  deriving newtype
    ( Show
    , DecodeScalar
    , EncodeScalar
    )

$(PPTH.makeAdaptorAndInstanceInferrable "pPKey" ''PKey')

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
          "TaskPKey"
      }

instance DB.DefaultFromField DB.SqlInt4 (PKey' Int) where
  defaultFromField = DB.unsafeFromField PKey (DB.defaultFromField @DB.PGInt4)

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
          "TaskUuid"
      }

--
-- Table Definition
--

data
  TaskRow'
    ownerId
    pkey
    uuid = TaskRow
  { ownerId' :: ownerId
  , pkey' :: pkey
  , uuid' :: uuid
  }

$(PPTH.makeAdaptorAndInstanceInferrable "pTaskRow" ''TaskRow')

type TaskRow =
  DB.TimestampedRow
    ( TaskRow'
        (Maybe User.PKey) -- ownerId
        PKey -- pkey
        Uuid -- uuid
    )

type WriteField =
  DB.TimestampedWriteField
    ( TaskRow'
        (Maybe (DB.FieldNullable DB.PGInt4)) -- TODO: how do I make this use User.PKey???
        PKeyWriteField
        UuidWriteField
    )

type ReadField =
  DB.TimestampedReadField
    ( TaskRow'
        (DB.FieldNullable DB.PGInt4) -- TODO: how do I make this use User.PKey???
        PKeyReadField
        UuidReadField
    )

table :: DB.Table WriteField ReadField
table =
  DB.table "gigs" . DB.pTimestampedTable . DB.withTimestampFields $
    pTaskRow rowDef
  where
    rowDef =
      TaskRow
        { ownerId' = DB.tableField "owner_id" -- TODO: how do I make this use User.PKey???
        , pkey' = pPKey . PKey $ DB.readOnlyTableField "id"
        , uuid' = pUuid . Uuid $ DB.tableField "uuid"
        }

--
-- DATABASE QUERIES
--

select :: DB.Select ReadField
select = DB.selectTable table

findByUuid :: Uuid -> DB.Select ReadField
findByUuid taskUuid = do
  task <- select
  DB.where_ $ uuid' (DB.record task) .=== DB.toFields taskUuid
  return task

findByPKey :: PKey -> DB.Select ReadField
findByPKey pkey = do
  task <- select
  DB.where_ $ pkey' (DB.record task) .=== DB.toFields pkey
  return task

allByOwnerId :: User.PKey -> DB.Select ReadField
allByOwnerId ownerId = do
  task <- select
  -- TODO: how do I make this ownerId' into a User.PKey so I don't have to unwrap it to compare???
  DB.where_ $ ownerId' (DB.record task) .=== DB.toNullable (User.unPKey (DB.toFields ownerId))
  return task

--
-- GRAPHQL MODEL
--

data Task m = Task
  { uuid :: m Uuid
  , owner :: m (Maybe (User m))
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

--
-- # Resolvers
--
resolve ::
  (GQL.MonadGraphQL o m) => TaskRow -> GQL.Value o m Task
resolve task =
  let TaskRow{..} = DB.record task
   in pure $
        Task
          { uuid = pure uuid'
          , owner = maybe (pure Nothing) resolveOwnerByFKey (Arg <$> ownerId')
          }

resolveAll :: (GQL.MonadGraphQL o m) => GQL.Composed o m [] Task
resolveAll = GQL.runSelect $ do
  taskRow <- select
  let task@TaskRow{uuid', ownerId'} = DB.record taskRow
  ownerUser <- case ownerId' of
    Nothing -> pure Nothing
    Just id -> do
      user <- User.select
      DB.where_ $ DB.toFields id .=== DB.toFields (User.pkey' (DB.record user))
      return (Just user)
  return $
    Task
      { uuid = pure uuid'
      , owner = maybe (pure Nothing) (fmap Just . User.resolve) ownerUser
      }

resolveByUuid ::
  (GQL.MonadGraphQL o m) => Arg "uuid" Uuid -> GQL.Value o m Task
resolveByUuid (Arg uuid) = do
  let uuidStr = UUID.toString . Scalar.unUUID . unUuid $ uuid
  it <- GQL.runSelectOne (findByUuid uuid) ("An organization with UUID " <> uuidStr <> " does not exist.")
  resolve it

resolveByPKey ::
  (GQL.MonadGraphQL o m) => Arg "taskPKey" PKey -> GQL.Value o m Task
resolveByPKey (Arg taskPKey) = do
  let pkeyStr = show . unPKey $ taskPKey
  it <- GQL.runSelectOne (findByPKey taskPKey) ("An organization with ID " <> pkeyStr <> " does not exist.")
  resolve it

resolveMaybeByPKey :: (GQL.MonadGraphQL o m) => Arg "taskPKey" PKey -> GQL.Composed o m Maybe Task
resolveMaybeByPKey (Arg taskPKey) = do
  m <- GQL.runSelectMaybeOne (findByPKey taskPKey)
  maybe (pure Nothing) (fmap Just . resolve) m

resolveOwnerByFKey :: (GQL.MonadGraphQL o m) => Arg "ownerId" User.PKey -> GQL.Composed o m Maybe User
resolveOwnerByFKey (Arg ownerId) = do
  User.resolveMaybeByPKey (Arg ownerId)
