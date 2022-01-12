module Pinata.Graphql.Query where

import Data.Morpheus.Types (
  Arg,
  GQLType,
 )
import GHC.Generics (Generic)
import Pinata.Model.Organization (
  Organization,
  Uuid,
 )
import Pinata.Model.Scalar (UUID)
import Pinata.Model.Task (Task)

data Query (m :: * -> *) = Query
  { organization :: Arg "uuid" Uuid -> m (Organization m)
  , tasks :: m [Task m]
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)
