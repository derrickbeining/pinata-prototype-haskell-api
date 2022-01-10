module Pinata.Graphql.Query where

import Data.Morpheus.Types (
  Arg,
  GQLType,
 )
import GHC.Generics (Generic)
import Pinata.Model.Organization (
  Organization,
  OrganizationUUID,
 )
import Pinata.Model.Scalar (UUID)

newtype Query (m :: * -> *) = Query
  { organization :: Arg "uuid" OrganizationUUID -> m (Organization m)
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)
