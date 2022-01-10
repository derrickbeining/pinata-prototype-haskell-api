module Pinata.Model.Program where

import Data.Morpheus.Kind ( SCALAR )
import Data.Morpheus.Types
       ( DecodeScalar
       , EncodeScalar
       , GQLType(..)
       )
import Data.Text ( Text )

import GHC.Generics ( Generic )

import Pinata.Model.Scalar ( UUID )

newtype ProgramId = ProgramId UUID
    deriving stock ( Show
                   , Generic )
    deriving newtype ( DecodeScalar
                     , EncodeScalar )

instance GQLType ProgramId where
    type KIND ProgramId = SCALAR

data Program m =
    Program
    { id :: m ProgramId
    , name :: m Text
    }
    deriving stock ( Generic )
    deriving anyclass ( GQLType )
