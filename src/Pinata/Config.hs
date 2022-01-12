module Pinata.Config where

import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.URL as PG
import GHC.Generics (Generic)
import LoadEnv (loadEnv)
import System.Environment (lookupEnv)
import qualified System.Envy as Env

-- |

-------------------------------------------------------------------------------
data Config = Config
  { databaseUrl :: Text
  , jwtSecret :: Text
  , port :: Int
  }
  deriving (Generic, Show)

instance Env.DefConfig Config where
  defConfig =
    Config
      { databaseUrl =
          "postgresql://postgres:password@localhost:10000/pinata_development"
      , jwtSecret = "my_jwt_secret"
      , port = 10092
      }

instance Env.FromEnv Config

-------------------------------------------------------------------------------
type Init a = ExceptT String IO a

loadConfig :: Init Config
loadConfig = ExceptT $ liftIO $ loadEnv >> Env.decodeEnv

createConnectionsPool :: Config -> Init (Pool PG.Connection)
createConnectionsPool config =
  case PG.parseDatabaseUrl . T.unpack . databaseUrl $ config of
    Just connectionInfo ->
      liftIO $ Pool.createPool (PG.connect connectionInfo) PG.close 2 5 10
    _ -> throwError "Invalid database url"

initialize :: Init (Config, Pool PG.Connection)
initialize = do
  config <- loadConfig
  connectionPool <- createConnectionsPool config
  return (config, connectionPool)
