module Pinata.Graphql.Resolver where

import Data.Maybe (fromMaybe)
import Data.Morpheus.NamedResolvers (resolve)
import Data.Morpheus.Types

import Pinata.Graphql
import Pinata.Graphql.Query (Query (..))
import qualified Pinata.Model.Organization as Org

root :: RootResolver Web () Query Undefined Undefined
root =
  RootResolver
    { queryResolver
    , mutationResolver
    , subscriptionResolver
    }
  where
    queryResolver =
      Query
        { organization = Org.organizationByUUIDResolver
        }

    mutationResolver = Undefined

    subscriptionResolver = Undefined
