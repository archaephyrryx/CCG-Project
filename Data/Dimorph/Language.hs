{-# LANGUAGE DeriveDataTypeable #-}
module Data.Dimorph.Language where

import Data.Data
import Data.Typeable

type TName = String
type CName = String

data Iso = Iso TName TName
    deriving (Data, Typeable, Show, Eq)

data Term = Unary CName
    deriving (Data, Typeable, Show, Eq)

newtype LHS = LHS { getL :: Term }
    deriving (Data, Typeable, Show, Eq)

newtype RHS = RHS { getR :: Term }
    deriving (Data, Typeable, Show, Eq)

data QMapping = QMapping LHS RHS
    deriving (Data, Typeable, Show, Eq)

data MDef = MDef Iso [QMapping]
    deriving (Data, Typeable, Show)
