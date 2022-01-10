module Pinata.Model.UICustomization where

import Data.Morpheus.Kind ( SCALAR )
import Data.Morpheus.Types
       ( DecodeScalar
       , EncodeScalar
       , GQLType(..)
       )

import GHC.Generics ( Generic )

import Pinata.Model.Scalar ( UUID )

newtype UICustomizationId = UICustomizationId UUID
    deriving stock ( Show
                   , Generic )
    deriving newtype ( DecodeScalar
                     , EncodeScalar )

instance GQLType UICustomizationId where
    type KIND UICustomizationId = SCALAR

newtype UICustomization m =
    UICustomization
    { id :: m UICustomizationId
    }
    deriving stock ( Generic )
    deriving anyclass ( GQLType )
