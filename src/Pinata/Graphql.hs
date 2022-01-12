module Pinata.Graphql where

import Pinata.Auth.JWT
import Pinata.Auth.Password

import Pinata.Config

import Control.Monad.Base (MonadBase)
import Control.Monad.Except (
  ExceptT,
  MonadError,
  throwError,
 )
import Control.Monad.Reader (
  MonadReader,
  ReaderT,
  asks,
 )
import Control.Monad.Trans (
  MonadIO,
  MonadTrans,
  liftIO,
 )
import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (listToMaybe)
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Types (
  ComposedResolver,
  ID (ID),
  Resolver,
  ResolverO,
  WithOperation,
  lift,
 )
import qualified Data.Morpheus.Types as Morpheus
import Data.Morpheus.Types.Internal.AST (
  OperationType,
 )
import Data.Pool (
  Pool,
  withResource,
 )
import Data.Profunctor.Product.Default (Default)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock (getCurrentTime)
import qualified Data.Time.Clock.POSIX as Time
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PG
import GHC.Int (Int64)
import Network.HTTP.Types (Status)
import Opaleye (
  FromFields,
  Insert,
  Select,
  Update,
 )
import qualified Opaleye
import Pinata.Error
import Pinata.Model.Scalar

--
-------------------------------------------------------------------------------
data Env = Env
  { dbPool :: Pool Connection
  , config :: Config
  , currentUserId :: Maybe Int
  }

newtype Web a = Web
  { runWeb :: ExceptT Error (ReaderT Env IO) a
  }
  deriving stock (Functor)
  deriving newtype
    ( Applicative
    , Monad
    , MonadReader Env
    , MonadError Error
    , MonadIO
    , MonadBase IO
    , MonadBaseControl IO
    )

-- | constraints for performing all operations in our `Web` monad transformer stack
type MonadWeb m =
  ( Applicative m
  , Monad m
  , MonadReader Env m
  , MonadError Error m
  , MonadIO m
  , MonadBase IO m
  , MonadBaseControl IO m
  )

type MonadGraphQL o m =
  ( MonadIO (Resolver o () m)
  , WithOperation o
  , MonadTrans (Resolver o ())
  )

type MonadWebGraphQL o m =
  ( MonadWeb m
  , MonadGraphQL o m
  )

-- | Resolve `m value`
type Value (o :: OperationType) (m :: * -> *) (a :: k) =
  MonadWeb m => ResolverO o () m a

-- | Resolve `m (f value)`
type Composed (o :: OperationType) (m :: * -> *) f (a :: k) =
  MonadWeb m => ComposedResolver o () m f a

-------------------------------------------------------------------------------

runSelect ::
  MonadGraphQL o m =>
  Default FromFields fields haskells =>
  Select fields ->
  Value o m [haskells]
runSelect select = do
  db <- lift $ asks dbPool
  liftIO $ withResource db $ \connection -> Opaleye.runSelect connection select

runSelectMaybeOne ::
  MonadGraphQL o m =>
  Default FromFields fields haskells =>
  Select fields ->
  Value o m (Maybe haskells)
runSelectMaybeOne select = do
  db <- lift $ asks dbPool
  xs <- liftIO $ withResource db $ \connection -> Opaleye.runSelect connection select
  pure $ listToMaybe xs

runSelectOne ::
  MonadGraphQL o m =>
  Default FromFields fields haskells =>
  Select fields ->
  String ->
  Value o m haskells
runSelectOne select errorMsg = do
  res <- runSelectMaybeOne select
  case res of
    Nothing -> fail errorMsg
    Just x -> return x

-------------------------------------------------------------------------------

-- |
runInsert :: MonadGraphQL o m => Insert haskells -> Value o m haskells
runInsert insert = do
  db <- lift $ asks dbPool
  liftIO $ withResource db $ \connection -> Opaleye.runInsert_ connection insert

-------------------------------------------------------------------------------
runUpdate :: MonadGraphQL o m => Update haskells -> Value o m haskells
runUpdate update = do
  db <- lift $ asks dbPool
  liftIO $ withResource db $ \connection -> Opaleye.runUpdate_ connection update

-------------------------------------------------------------------------------
requireAuthorized :: MonadGraphQL o m => Value o m Int
requireAuthorized = do
  maybeID <- lift $ asks currentUserId
  case maybeID of
    Just id -> return id
    _ -> lift $ throwError $ simpleError "Unauthorized"
