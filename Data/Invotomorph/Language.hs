{-# LANGUAGE DeriveDataTypeable #-}
module Data.Invotomorph.Language where

import Data.Data
import Data.Typeable

type TName = String
type CName = String

data Auto = Auto TName
    deriving (Data, Typeable, Show, Eq)

data Term = Unary CName
    deriving (Data, Typeable, Show, Eq)

newtype LHS = LHS { getL :: Term }
    deriving (Data, Typeable, Show, Eq)

newtype RHS = RHS { getR :: Term }
    deriving (Data, Typeable, Show, Eq)

data QRule = QRule LHS RHS
    deriving (Data, Typeable, Show, Eq)

data RDef = RDef Auto [QRule]
    deriving (Data, Typeable, Show)
