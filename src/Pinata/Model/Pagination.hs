module Pinata.Model.Pagination where

import Data.Morpheus.Types ( GQLType )

import GHC.Generics ( Generic )

data ConnectionPageInfo cursor m =
    ConnectionPageInfo
    { hasNextPage :: m Bool
    , offset :: m (Maybe Word)
    , endCursor :: m cursor
    , totalCount :: m Word
    }
    deriving stock ( Generic )
    deriving anyclass ( GQLType )

data PaginatedEdge cursor entity monad =
    PaginatedEdge
    { cursor :: Maybe cursor
    , node :: monad entity
    }
    deriving stock ( Generic )
    deriving anyclass ( GQLType )

data PaginatedEntity cursor entity monad =
    PaginatedEntity
    { edges :: monad [ PaginatedEdge cursor entity monad ]
    , pageInfo :: monad (ConnectionPageInfo cursor monad)
    }
    deriving stock ( Generic )
    deriving anyclass ( GQLType )
