module Pinata.Model.Organization (
  OrderingOrganizationEnum,
  OrderingOrganizationInput,
  OrgCreatorsArgs,
  OrgProgramsArgs,
  OrgRolesArgs,
  Organization,
  OrganizationInput,
  OrganizationType,
  OrganizationUUID,
  PaginatedOrganization,
  PaginatedOrganizationEdge,
  organizationByUUIDResolver,
  organizationResolver,
) where

import qualified Control.Monad.Trans.Except as ExceptT
import qualified Data.Aeson as Aeson
import Data.Data (Proxy (Proxy), Typeable)
import Data.Functor ((<&>))
import Data.Morpheus.Kind (SCALAR)
import Data.Morpheus.Types (
  Arg (Arg, argValue),
  DecodeScalar,
  EncodeScalar,
  GQLType (..),
  QUERY,
 )
import qualified Data.Morpheus.Types as Mor
import Data.Profunctor.Product.Default (Default (..))
import qualified Data.Profunctor.Product.TH as PPTH
import qualified Data.Tagged as Tag
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Opaleye ((.===))
import qualified Opaleye as DB
import qualified Pinata.DB as DB
import Pinata.Graphql (
  GraphQL,
  Value,
  Web,
  runSelectOne,
 )
import Pinata.Model.Basics (Direction)
import Pinata.Model.BillingContact (BillingContact)
import Pinata.Model.Industry (Industry)
import Pinata.Model.Option (Option)
import Pinata.Model.Pagination (
  PaginatedEdge,
  PaginatedEntity,
 )
import Pinata.Model.Product (Product)
import Pinata.Model.Program (Program)
import Pinata.Model.Role (Role)
import qualified Pinata.Model.Scalar as Scalar
import Pinata.Model.UICustomization (UICustomization)
import Pinata.Model.User (User, UserPKey (..), UserPKey' (..), pUserPKey)
import qualified Pinata.Model.User as User

--
-- DATABASE MODEL
--

newtype OrganizationUUID' a = OrganizationUUID
  { unOrganizationUUID :: a
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

$(PPTH.makeAdaptorAndInstanceInferrable "pOrganizationUUID" ''OrganizationUUID')

instance Typeable a => GQLType (OrganizationUUID' a) where
  type KIND (OrganizationUUID' a) = SCALAR

type OrganizationUUID = OrganizationUUID' Scalar.UUID

type OrganizationUUIDReadField =
  OrganizationUUID' (DB.Field DB.PGUuid)

-- | use Maybe because we don't need to specify uuid when inserting
type OrganizationUUIDWriteField =
  OrganizationUUID' (Maybe (DB.Field DB.PGUuid))

-- instance DB.IsSqlType a => DB.IsSqlType (OrganizationUUID' a) where
--   showSqlType _ = DB.showSqlType (Proxy @a)

-- instance Default DB.ToFields OrganizationUUID (DB.Column DB.PGUuid) where
--   def = DB.toToFields (DB.sqlUUID . unOrganizationUUID)

-- instance DB.DefaultFromField DB.PGUuid OrganizationUUID where
--   defaultFromField = DB.unsafeFromField OrganizationUUID (DB.defaultFromField @DB.PGUuid)

--
--
--

newtype OrganizationPKey' a = OrganizationPKey
  { unOrganizationPKey :: a
  }
  deriving stock
    ( Generic
    , Show
    , Eq
    )

$(PPTH.makeAdaptorAndInstanceInferrable "pOrganizationPKey" ''OrganizationPKey')

type OrganizationPKey = OrganizationPKey' Int

type OrganizationPKeyReadField =
  OrganizationPKey' (DB.Field DB.PGInt4)

type OrganizationPKeyWriteField =
  OrganizationPKey' () -- Disallow writing to the pkey column

-- instance DB.IsSqlType a => DB.IsSqlType (OrganizationPKey' a) where
--   showSqlType _ = DB.showSqlType (Proxy @a)

-- instance Default DB.ToFields OrganizationPKey (DB.Column DB.PGInt4) where
--   def = DB.toToFields (DB.sqlInt4 . fromIntegral . unOrganizationPKey)

-- instance DB.DefaultFromField DB.PGInt4 OrganizationPKey where
--   defaultFromField = DB.unsafeFromField OrganizationPKey (DB.defaultFromField @DB.PGInt4)

--
-- Table Definition
--

data
  OrganizationRow'
    activatedAt --
    activatedBy
    adminLabel
    displayName
    name
    pkey
    uuid = OrganizationRow
  { activatedAt' :: activatedAt
  , activatedBy' :: activatedBy
  , adminLabel' :: adminLabel
  , displayName' :: displayName
  , name' :: name
  , pkey' :: pkey
  , uuid' :: uuid
  }

$(PPTH.makeAdaptorAndInstanceInferrable "pOrganizationRow" ''OrganizationRow')

type OrganizationRow =
  DB.TimestampedRow
    ( OrganizationRow'
        (Maybe Scalar.UtcTimestamp) -- activatedAt
        (Maybe UserPKey) -- activatedBy
        (Maybe Text) -- adminLabel
        (Maybe Text) -- displayName
        Text -- name
        OrganizationPKey -- pkey
        OrganizationUUID -- uuid
    )

type OrganizationWriteField =
  DB.TimestampedWriteField
    ( OrganizationRow'
        (DB.FieldNullable DB.PGTimestamptz)
        (DB.FieldNullable DB.PGInt4)
        (DB.FieldNullable DB.PGText)
        (DB.FieldNullable DB.PGText)
        (DB.Field DB.PGText)
        OrganizationPKeyWriteField
        OrganizationUUIDWriteField
    )

type OrganizationReadField =
  DB.TimestampedReadField
    ( OrganizationRow'
        (DB.FieldNullable DB.PGTimestamptz)
        (DB.FieldNullable DB.PGInt4)
        (DB.FieldNullable DB.PGText)
        (DB.FieldNullable DB.PGText)
        (DB.Field DB.PGText)
        OrganizationPKeyReadField
        OrganizationUUIDReadField
    )

table :: DB.Table OrganizationWriteField OrganizationReadField
table =
  DB.table "organizations" . DB.pTimestampedTable . DB.withTimestampFields $
    pOrganizationRow rowDef
  where
    rowDef =
      OrganizationRow
        { activatedAt' = DB.tableField "activated_at"
        , activatedBy' = DB.tableField "activated_by"
        , adminLabel' = DB.tableField "admin_label"
        , displayName' = DB.tableField "display_name"
        , name' = DB.tableField "name"
        , pkey' = pOrganizationPKey . OrganizationPKey $ DB.readOnlyTableField "id"
        , uuid' = pOrganizationUUID . OrganizationUUID $ DB.tableField "uuid"
        }

--
-- DATABASE QUERIES
--

select :: DB.Select OrganizationReadField
select = DB.selectTable table

findByUUID :: OrganizationUUID -> DB.Select OrganizationReadField
findByUUID orgUuid = do
  org <- select
  DB.where_ $ uuid' (DB.record org) .=== DB.toFields orgUuid
  return org

--
-- GRAPHQL MODEL
--

data Organization m = Organization
  { activatedAt :: m (Maybe Scalar.UtcTimestamp)
  , activatedBy :: m (Maybe (User m))
  , adminLabel :: m (Maybe Text)
  , uuid :: m OrganizationUUID
  , name :: m Text
  , displayName :: m (Maybe Text)
  , createdAt :: m Scalar.UtcTimestamp
  , updatedAt :: m Scalar.UtcTimestamp
  -- , active :: m Bool
  -- , availabilityCalendarActive :: m (Maybe Bool)
  -- , billingContacts :: m [BillingContact m]
  -- , brandingActive :: m Bool
  -- , creators :: OrgCreatorsArgs -> m [User m]
  -- , description :: m (Maybe Text)
  -- , financialPackageActive :: m Bool
  -- , gigRequestsActive :: m Bool
  -- , gigSelfAssignmentActive :: m Bool
  -- , hasAcceptedTerms :: m Bool
  -- , hasInvitedUsers :: m Bool
  -- , industry :: m (Industry m)
  -- , invoicingActive :: m Bool
  -- , isWorkspace :: m Bool
  -- , oneOffLocations :: m Bool
  -- , partnershipsVisible :: m Bool
  -- , products :: m [Product m]
  -- , programGroupsVisible :: m Bool
  -- , programs :: OrgProgramsArgs -> m [Program m]
  -- , roles :: OrgRolesArgs -> m [Role m]
  -- , roster :: m [User m]
  -- , uiCustomization :: m (UICustomization m)
  -- , website :: m (Maybe Text)
  -- , workspacesActive :: m Bool
  -- , type' :: m OrganizationType
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

newtype OrgProgramsArgs = OrgProgramsArgs
  { includeArchived :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

data OrgRolesArgs = OrgRolesArgs
  { active :: Bool
  , pseudo :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

newtype OrgCreatorsArgs = OrgCreatorsArgs
  { programIds :: [Scalar.UUID] -- --------------------------------------------------
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

data OrganizationInput m = OrganizationInput
  { name :: m Text
  , website :: m (Maybe Text)
  , description :: m (Maybe Text)
  , uiCustomizationId :: m (Maybe Scalar.UUID)
  , industryId :: m (Maybe Scalar.UUID)
  , industryKey :: m (Maybe Text)
  , teamSizeKey :: m (Maybe Text)
  , hasAcceptedTerms :: m Bool
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

data OrganizationType
  = ClientOrgType
  | AgencyOrgType
  deriving stock (Generic)
  deriving anyclass (GQLType)

type PaginatedOrganizationEdge m = PaginatedEdge OrganizationUUID m

type PaginatedOrganization m =
  PaginatedEntity OrganizationUUID (Organization m) m

data OrderingOrganizationEnum = OrderingOrgName
  deriving stock (Generic)
  deriving anyclass (GQLType)

data OrderingOrganizationInput = OrderingOrganizationInput
  { sort :: OrderingOrganizationEnum
  , direction :: Direction
  , args :: Aeson.Value
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

--
-- # Resolvers
--
organizationResolver ::
  (Monad m, GraphQL o) =>
  OrganizationRow ->
  Value o (Organization m)
organizationResolver org =
  let OrganizationRow{..} = DB.record org
   in do
        pure
          Organization
            { activatedAt = pure activatedAt'
            , activatedBy = error "Not implemented"
            , adminLabel = pure adminLabel'
            , createdAt = pure $ Scalar.UtcTimestamp $ DB.recordCreatedAt org
            , displayName = pure displayName'
            , name = pure name'
            , updatedAt = pure $ Scalar.UtcTimestamp $ DB.recordUpdatedAt org
            , uuid = pure uuid'
            }

organizationByUUIDResolver ::
  (GraphQL o, Monad m) =>
  Arg "uuid" OrganizationUUID ->
  Value o (Organization m)
organizationByUUIDResolver uuidArg = do
  let orgUUID = argValue uuidArg
      uuidStr = UUID.toString . Scalar.unUUID . unOrganizationUUID $ orgUUID
      q = findByUUID . argValue $ uuidArg
  it <- runSelectOne q ("An organization with UUID " <> uuidStr <> " does not exist.")
  organizationResolver it
