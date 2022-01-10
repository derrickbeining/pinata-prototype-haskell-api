module Pinata.Config where

import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.URL
import GHC.Generics
import LoadEnv
import System.Environment (lookupEnv)
import System.Envy

-- |

-------------------------------------------------------------------------------
data Config = Config
  { databaseUrl :: Text
  , jwtSecret :: Text
  , port :: Int
  }
  deriving (Generic, Show)

instance DefConfig Config where
  defConfig =
    Config
      { databaseUrl =
          "postgresql://postgres:password@localhost:10000/pinata_development"
      , jwtSecret = "my_jwt_secret"
      , port = 10092
      }

instance FromEnv Config

-------------------------------------------------------------------------------
type Init a = ExceptT String IO a

loadConfig :: Init Config
loadConfig = ExceptT $ liftIO $ loadEnv >> decodeEnv

createConnectionsPool :: Config -> Init (Pool Connection)
createConnectionsPool config =
  case parseDatabaseUrl . T.unpack . databaseUrl $ config of
    Just connectionInfo ->
      liftIO $ createPool (connect connectionInfo) close 2 5 10
    _ -> throwError "Invalid database url"

initialize :: Init (Config, Pool Connection)
initialize = do
  config <- loadConfig
  connectionPool <- createConnectionsPool config
  return (config, connectionPool)
