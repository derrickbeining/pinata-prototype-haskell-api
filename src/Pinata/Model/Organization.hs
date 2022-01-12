module Pinata.Model.Organization (
  OrderingOrganizationEnum,
  OrderingOrganizationInput,
  OrgCreatorsArgs,
  OrgProgramsArgs,
  OrgRolesArgs,
  Organization,
  OrganizationInput,
  OrganizationType,
  Uuid,
  PaginatedOrganization,
  PaginatedOrganizationEdge,
  resolveByUuid,
  resolve,
) where

import Control.Exception (try)
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Data.Aeson as Aeson
import Data.Data (Proxy (Proxy), Typeable)
import Data.Functor ((<&>))
import Data.Morpheus.Kind (SCALAR)
import qualified Data.Morpheus.Kind as GQL
import Data.Morpheus.Types (
  Arg (Arg, argValue),
  DecodeScalar,
  EncodeScalar,
  GQLType (..),
  QUERY,
  Resolver,
 )
import qualified Data.Morpheus.Types as GQL
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
import qualified Pinata.Graphql as GQL
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
import Pinata.Model.User (User)
import qualified Pinata.Model.User as User

--
-- DATABASE MODEL
--

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

instance Typeable a => GQLType (Uuid' a) where
  type KIND (Uuid' a) = SCALAR
  typeOptions _ opts =
    opts
      { GQL.typeNameModifier = \isInput name ->
          "OrganizationUuid"
      }

type UuidReadField =
  Uuid' (DB.Field DB.PGUuid)

-- | use Maybe because we don't need to specify uuid when inserting
type UuidWriteField =
  Uuid' (Maybe (DB.Field DB.PGUuid))

-- instance DB.IsSqlType a => DB.IsSqlType (Uuid' a) where
--   showSqlType _ = DB.showSqlType (Proxy @a)

-- instance Default DB.ToFields Uuid (DB.Column DB.PGUuid) where
--   def = DB.toToFields (DB.sqlUUID . unUuid)

-- instance DB.DefaultFromField DB.PGUuid Uuid where
--   defaultFromField = DB.unsafeFromField Uuid (DB.defaultFromField @DB.PGUuid)

--
--
--

newtype PKey' a = PKey
  { unPKey :: a
  }
  deriving stock
    ( Generic
    , Show
    , Eq
    )

$(PPTH.makeAdaptorAndInstanceInferrable "pPKey" ''PKey')

type PKey = PKey' Int

instance Typeable a => GQLType (PKey' a) where
  type KIND (PKey' a) = SCALAR
  typeOptions _ opts =
    opts
      { GQL.typeNameModifier = \isInput name ->
          "OrganizationPKey"
      }

type PKeyReadField =
  PKey' (DB.Field DB.PGInt4)

type PKeyWriteField =
  PKey' () -- Disallow writing to the pkey column

-- instance DB.IsSqlType a => DB.IsSqlType (PKey' a) where
--   showSqlType _ = DB.showSqlType (Proxy @a)

-- instance Default DB.ToFields PKey (DB.Column DB.PGInt4) where
--   def = DB.toToFields (DB.sqlInt4 . fromIntegral . unPKey)

-- instance DB.DefaultFromField DB.PGInt4 PKey where
--   defaultFromField = DB.unsafeFromField PKey (DB.defaultFromField @DB.PGInt4)

--
-- Table Definition
--

data
  OrganizationRow'
    activatedAt --
    activatedBy
    active
    adminLabel
    availabilityCalendarActive
    brandingActive
    displayName
    name
    pkey
    uuid --
  = OrganizationRow
  { activatedAt' :: activatedAt
  , activatedBy' :: activatedBy
  , active' :: active
  , availabilityCalendarActive' :: availabilityCalendarActive
  , adminLabel' :: adminLabel
  , brandingActive' :: brandingActive
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
        (Maybe User.PKey) -- activatedBy
        Bool -- active
        (Maybe Text) -- adminLabel
        (Maybe Bool) -- availabilityCalendarActive
        Bool -- brandingActive
        (Maybe Text) -- displayName
        Text -- name
        PKey -- pkey
        Uuid -- uuid
    )

type WriteField =
  DB.TimestampedWriteField
    ( OrganizationRow'
        (DB.FieldNullable DB.PGTimestamptz)
        (DB.FieldNullable DB.PGInt4)
        (Maybe (DB.Field DB.PGBool))
        (DB.FieldNullable DB.PGText)
        (Maybe (DB.FieldNullable DB.PGBool))
        (Maybe (DB.Field DB.PGBool))
        (DB.FieldNullable DB.PGText)
        (DB.Field DB.PGText)
        PKeyWriteField
        UuidWriteField
    )

type ReadField =
  DB.TimestampedReadField
    ( OrganizationRow'
        (DB.FieldNullable DB.PGTimestamptz)
        (DB.FieldNullable DB.PGInt4)
        (DB.Field DB.PGBool)
        (DB.FieldNullable DB.PGText)
        (DB.FieldNullable DB.PGBool)
        (DB.Field DB.PGBool)
        (DB.FieldNullable DB.PGText)
        (DB.Field DB.PGText)
        PKeyReadField
        UuidReadField
    )

table :: DB.Table WriteField ReadField
table =
  DB.table "organizations" . DB.pTimestampedTable . DB.withTimestampFields $
    pOrganizationRow rowDef
  where
    rowDef =
      OrganizationRow
        { activatedAt' = DB.tableField "activated_at"
        , activatedBy' = DB.tableField "activated_by"
        , active' = DB.tableField "active"
        , adminLabel' = DB.tableField "admin_label"
        , availabilityCalendarActive' = DB.tableField "availability_calendar_active"
        , brandingActive' = DB.tableField "branding_active"
        , displayName' = DB.tableField "display_name"
        , name' = DB.tableField "name"
        , pkey' = pPKey . PKey $ DB.readOnlyTableField "id"
        , uuid' = pUuid . Uuid $ DB.tableField "uuid"
        }

--
-- DATABASE QUERIES
--

select :: DB.Select ReadField
select = DB.selectTable table

findByUUID :: Uuid -> DB.Select ReadField
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
  , active :: m Bool
  , adminLabel :: m (Maybe Text)
  , availabilityCalendarActive :: m (Maybe Bool)
  , brandingActive :: m Bool
  , createdAt :: m Scalar.UtcTimestamp
  , displayName :: m (Maybe Text)
  , name :: m Text
  , updatedAt :: m Scalar.UtcTimestamp
  , uuid :: m Uuid
  -- , billingContacts :: m [BillingContact m]
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

type PaginatedOrganizationEdge m = PaginatedEdge Uuid m

type PaginatedOrganization m =
  PaginatedEntity Uuid (Organization m) m

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
resolve ::
  (GQL.MonadGraphQL o m) =>
  OrganizationRow ->
  GQL.Value o m Organization
resolve org =
  let OrganizationRow{..} = DB.record org
   in do
        pure
          Organization
            { activatedAt = pure activatedAt'
            , activatedBy = maybe (pure Nothing) User.resolveMaybeByPKey (Arg <$> activatedBy')
            , active = pure active'
            , adminLabel = pure adminLabel'
            , availabilityCalendarActive = pure availabilityCalendarActive'
            , brandingActive = pure brandingActive'
            , createdAt = pure $ Scalar.UtcTimestamp $ DB.recordCreatedAt org
            , displayName = pure displayName'
            , name = pure name'
            , updatedAt = pure $ Scalar.UtcTimestamp $ DB.recordUpdatedAt org
            , uuid = pure uuid'
            }

resolveByUuid ::
  (GQL.MonadGraphQL o m) =>
  Arg "uuid" Uuid ->
  GQL.Value o m Organization
resolveByUuid uuidArg = do
  let orgUUID = argValue uuidArg
      uuidStr = UUID.toString . Scalar.unUUID . unUuid $ orgUUID
      q = findByUUID . argValue $ uuidArg
  it <- GQL.runSelectOne q ("An organization with UUID " <> uuidStr <> " does not exist.")
  resolve it
