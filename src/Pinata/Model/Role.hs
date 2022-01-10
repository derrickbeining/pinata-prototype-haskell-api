module Pinata.Model.Role where

import Data.Morpheus.Kind ( SCALAR )
import Data.Morpheus.Types
       ( DecodeScalar
       , EncodeScalar
       , GQLType(..)
       )
import Data.Text ( Text )

import GHC.Generics ( Generic )

import Pinata.Model.Scalar ( UUID )

newtype RoleId = RoleId UUID
    deriving stock ( Show
                   , Generic )
    deriving newtype ( DecodeScalar
                     , EncodeScalar )

instance GQLType RoleId where
    type KIND RoleId = SCALAR

data Role m =
    Role
    { id :: m RoleId
    , name :: m Text
    }
    deriving stock ( Generic )
    deriving anyclass ( GQLType )
