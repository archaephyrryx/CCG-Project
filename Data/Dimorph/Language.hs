{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE GADTs              #-}
module Data.Dimorph.Language where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Data
import Data.Typeable

newtype TName = TName { t :: Name }
  deriving (Data, Typeable, Show, Eq)
newtype CName = CName { c :: Name }
  deriving (Data, Typeable, Show, Eq)
newtype VName = VName { v :: Name }
  deriving (Data, Typeable, Show, Eq)

data Iso = Iso Isotype Isotype
    deriving (Data, Typeable, Show, Eq)

data Isotype = Poly VName
             | Mono TName
             | Bin TName VName --Isotype
             | Tern TName VName VName --Isotype Isotype
             deriving (Data, Typeable, Show, Eq)

data Compat = Compat
            | Incompat String
            deriving (Show, Read, Eq)

{-
tyVarCollect :: Isotype -> [VName]
tyVarCollect x = case x of
                   Poly x -> [x]
                   Mono _ -> []
                   Bin _ i -> tyVarCollect i
                   Tern _ i j -> (tyVarCollect i) ++ (tyVarCollect j)


tyCompat :: Isotype -> Isotype -> Compat
tyCompat t t' = case (t,t') of
                  (Poly (VName v), Poly (VName v')) ->
                    if v == v'
                       then Incompat "cannot define general `a -> a` dimorphism"
                       else Incompat "magical polymorphism cannot be defined"
                  (Mono _, Mono _) -> Compat
                  (Mono _,
-}


data Atom = AVar VName
          | ACon CName
          deriving (Data, Typeable, Show, Eq)

data Term = Constant Integer
          | Var VName
          | Unary CName
          | Binary CName Atom --Term
          | Ternary CName Atom Atom --Term Term
    deriving (Data, Typeable, Show, Eq)


termVarCollect :: Term -> [VName]
termVarCollect x = case x of
                     Var v -> [v]
                     Binary c a -> atomVar a
                     Ternary c a b -> atomVar a ++ atomVar b
                     _ -> []
  where
    atomVar :: Atom -> [VName]
    atomVar (AVar x) = [x]
    atomVar (ACon _) = []


{-
tmCompat :: Term -> Term -> Bool
-}

newtype LHS = LHS { getL :: Term }
    deriving (Data, Typeable, Show, Eq)

newtype RHS = RHS { getR :: Term }
    deriving (Data, Typeable, Show, Eq)

data QMapping = QMap LHS RHS
    deriving (Data, Typeable, Show, Eq)

data MDef = MDef Iso [QMapping]
    deriving (Data, Typeable, Show)

instance Lift CName where
  lift = return . AppE (ConE 'CName). ConE . c

instance Lift TName where
  lift = return . AppE (ConE 'TName). ConE . t

instance Lift VName where
  lift = return . AppE (ConE 'VName). ConE . v

instance Lift Isotype where
  lift (Mono tn) = do
    t <- lift tn
    return (AppE (ConE 'Mono) t)
  lift (Bin tn it) = do
    t <- lift tn
    i <- lift it
    return (AppE (AppE (ConE 'Bin) t) i)
  lift (Tern tn it jt) = do
    t <- lift tn
    i <- lift it
    j <- lift jt
    return (AppE (AppE (AppE (ConE 'Bin) t) i) j)

instance Lift Atom where
  lift (AVar v) = lift v
  lift (ACon c) = lift c

instance Lift Term where
  lift (Constant i) =
    return (AppE (ConE 'Constant) (LitE (IntegerL i)))
  lift (Var vn) = do
    v <- lift vn
    return (AppE (ConE 'Var) v)
  lift (Unary cn) = do
    c <- lift cn
    return (AppE (ConE 'Unary) c)
  lift (Binary cn vn) = do
    c <- lift cn
    v <- lift vn
    return (AppE (AppE (ConE 'Binary) c) v)
  lift (Ternary cn xn yn) = do
    c <- lift cn
    x <- lift xn
    y <- lift yn
    return (AppE (AppE (AppE (ConE 'Ternary) c) x) y)

instance Lift LHS where
  lift (LHS t) = do
    l <- lift t
    return (AppE (ConE 'LHS) l)

instance Lift RHS where
  lift (RHS t) = do
    r <- lift t
    return (AppE (ConE 'RHS) r)

instance Lift QMapping where
  lift (QMap l r) = do
    lh <- lift l
    rh <- lift r
    return (AppE (AppE (ConE 'QMap) (lh)) (rh))
