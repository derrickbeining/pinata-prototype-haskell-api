module Pinata.Model.Industry where

import Data.Morpheus.Kind ( SCALAR )
import Data.Morpheus.Types
       ( DecodeScalar
       , EncodeScalar
       , GQLType(..)
       )

import GHC.Generics ( Generic )

import Pinata.Model.Scalar ( UUID )

newtype IndustryId = IndustryId UUID
    deriving stock ( Show
                   , Generic )
    deriving newtype ( DecodeScalar
                     , EncodeScalar )

instance GQLType IndustryId where
    type KIND IndustryId = SCALAR

newtype Industry m =
    Industry
    { id :: m IndustryId
    }
    deriving stock ( Generic )
    deriving anyclass ( GQLType )
