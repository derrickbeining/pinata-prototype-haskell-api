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

import qualified Control.Arrow as Arrow
import Data.Data (Typeable)
import qualified Data.Morpheus as GQL
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types (
  Arg (Arg, argValue),
  DecodeScalar,
  EncodeScalar,
  GQLType (..),
 )
import qualified Data.Morpheus.Types as GQL
import Data.Profunctor.Product.Default (Default (..))
import qualified Data.Profunctor.Product.TH as PPTH
import Data.Text (Text)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Opaleye (fromPGSFromField, (.===))
import qualified Opaleye as DB
import qualified Opaleye.Internal.MaybeFields as DB
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
  DB.LocalTimestampedRow
    ( TaskRow'
        (Maybe User.PKey) -- ownerId
        PKey -- pkey
        Uuid -- uuid
    )

type WriteField =
  DB.LocalTimestampedWriteField
    ( TaskRow'
        (User.PKey' (Maybe (DB.FieldNullable DB.PGInt4))) -- TODO: how do I make this use User.PKey???
        PKeyWriteField
        UuidWriteField
    )

type ReadField =
  DB.LocalTimestampedReadField
    ( TaskRow'
        (User.PKey' (DB.FieldNullable DB.PGInt4)) -- TODO: how do I make this use User.PKey???
        PKeyReadField
        UuidReadField
    )

table :: DB.Table WriteField ReadField
table =
  DB.table "gigs" . DB.pTimestampedRow . DB.withLocalTimestampFields $
    pTaskRow rowDef
  where
    rowDef =
      TaskRow
        { ownerId' = User.pPKeyTableField $ DB.tableField "owner_id" -- TODO: how do I make this use User.PKey???
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
  DB.where_ $ ownerId' (DB.record task) .=== (DB.toNullable <$> DB.toFields ownerId)
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

resolveAll :: forall o m. (GQL.MonadGraphQL o m) => GQL.Composed o m [] Task
resolveAll = do
  rows <- getRows
  pure $
    flip fmap rows $ \(task, mUser) ->
      Task
        { uuid = pure $ uuid' $ DB.record task
        , owner = maybe (pure Nothing) (fmap Just . User.resolve) mUser
        }
  where
    getRows :: GQL.Composed o m [] (TaskRow, Maybe User.UserRow)
    getRows = GQL.runSelect query

    query :: DB.Select (ReadField, DB.MaybeFields User.ReadField)
    query = do
      task <- select
      mOwner <-
        DB.optionalRestrict User.select
          `DB.viaLateral` userIsOwner (ownerId' (DB.record task))
      pure (task, mOwner)

    userIsOwner :: User.PKey' (DB.Column (DB.Nullable DB.SqlInt4)) -> User.ReadField -> DB.Field DB.SqlBool
    userIsOwner ownerIdField u =
      ownerIdField .=== (DB.toNullable <$> User.pkey' (DB.record u))

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
