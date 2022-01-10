module Pinata.Model.Product where

import Data.Morpheus.Kind ( SCALAR )
import Data.Morpheus.Types
       ( DecodeScalar
       , EncodeScalar
       , GQLType(..)
       )
import Data.Text ( Text )

import GHC.Generics ( Generic )

import Pinata.Model.Scalar ( UUID )

newtype ProductId = ProductId UUID
    deriving stock ( Show
                   , Generic )
    deriving newtype ( DecodeScalar
                     , EncodeScalar )

instance GQLType ProductId where
    type KIND ProductId = SCALAR

data Product m =
    Product
    { id :: m ProductId
    , name :: m Text
    }
    deriving stock ( Generic )
    deriving anyclass ( GQLType )
