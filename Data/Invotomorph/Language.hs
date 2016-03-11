{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Data.Invotomorph.Language where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Data
import Data.Typeable

newtype TName = TName { t :: String }
  deriving (Data, Typeable, Show, Eq)
newtype CName = CName { c :: String }
  deriving (Data, Typeable, Show, Eq)

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


instance Lift CName where
  lift = return . AppE (ConE 'CName). LitE . StringL . c

instance Lift TName where
  lift = return . AppE (ConE 'TName). LitE . StringL . t

instance Lift Term where
  lift (Unary cn) = do
    c <- lift cn
    return (AppE (ConE 'Unary) c)

instance Lift LHS where
  lift (LHS t) = do
    l <- lift t
    return (AppE (ConE 'LHS) l)

instance Lift RHS where
  lift (RHS t) = do
    r <- lift t
    return (AppE (ConE 'RHS) r)

instance Lift QRule where
  lift (QRule l r) = do
    lh <- lift l
    rh <- lift r
    return (AppE (AppE (ConE 'QRule) (lh)) (rh))
