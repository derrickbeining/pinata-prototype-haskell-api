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

import Pinata.Error

import GHC.Int (Int64)

import Pinata.Model.Scalar

import Network.HTTP.Types (Status)

import Opaleye (
  FromFields,
  Insert,
  Select,
  Update,
 )
import qualified Opaleye

import Data.Maybe (listToMaybe)
import Prelude hiding (Ordering)

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

-- | Resolve single value
type Value (o :: OperationType) (a :: k) = ResolverO o () Web a

-- | Resolve (f value)
type Composed (o :: OperationType) f (a :: k) = ComposedResolver o () Web f a

type GraphQL o =
  ( MonadIO (Resolver o () Web)
  , WithOperation o
  , MonadTrans (Resolver o ())
  )

-------------------------------------------------------------------------------

-- |
runSelect ::
  GraphQL o =>
  Default FromFields fields haskells =>
  Select fields ->
  Value o [haskells]
runSelect select = do
  db <- lift $ asks dbPool
  liftIO $ withResource db $ \connection -> Opaleye.runSelect connection select

-------------------------------------------------------------------------------
runSelectMaybeOne ::
  GraphQL o =>
  Default FromFields fields haskells =>
  Select fields ->
  Value o (Maybe haskells)
runSelectMaybeOne select = do
  db <- lift $ asks dbPool
  xs <- liftIO $ withResource db $ \connection -> Opaleye.runSelect connection select
  pure $ listToMaybe xs

runSelectOne ::
  GraphQL o =>
  Default FromFields fields haskells =>
  Select fields ->
  String ->
  Value o haskells
runSelectOne select errorMsg = do
  res <- runSelectMaybeOne select
  case res of
    Nothing -> fail errorMsg
    Just x -> return x

-------------------------------------------------------------------------------

-- |
runInsert :: GraphQL o => Insert haskells -> Value o haskells
runInsert insert = do
  db <- lift $ asks dbPool
  liftIO $ withResource db $ \connection -> Opaleye.runInsert_ connection insert

-------------------------------------------------------------------------------
runUpdate :: GraphQL o => Update haskells -> Value o haskells
runUpdate update = do
  db <- lift $ asks dbPool
  liftIO $ withResource db $ \connection -> Opaleye.runUpdate_ connection update

-------------------------------------------------------------------------------
requireAuthorized :: GraphQL o => Value o Int
requireAuthorized = do
  maybeID <- lift $ asks currentUserId
  case maybeID of
    Just id -> return id
    _ -> lift $ throwError $ simpleError "Unauthorized"
