{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- | Common elements between the Invotomorph and Dimorph Abstract Syntax, based on Theseus
module Data.Morphism.Language where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Data
import Data.Typeable
import Util.TH
import Control.Applicative ((<$>),(<*>))

-- * Names
-- | `TName`, `CName`, and `VName` are all representations of named elements in the Morphism
-- declaration language. They are all newtypes based on the TemplateHaskell `Name` type, but refer
-- to different syntactical elements: types in morphism signatures (`TName`), constructors in
-- morphism bodies (`CNames`), and variables in either signatures or bodies (`VName`).

newtype TName = TName { t :: Name }
  deriving (Data, Typeable, Show, Eq)

newtype CName = CName { c :: Name }
  deriving (Data, Typeable, Show, Eq)

newtype VName = VName { v :: Name }
  deriving (Data, Typeable, Show, Eq)

instance Lift TName where
  lift (TName t) = return $ ConE 'TName $@ ConE t
instance Lift CName where
  lift (CName c) = return $ ConE 'CName $@ ConE c
instance Lift VName where
  lift (VName v) = return $ ConE 'VName $@ ConE v

-- * Types

-- | `Isotype` is a data-type representing the types of the left- or right-hand sides of a Dimorph
-- declaration, or the uniform mirrored type of an Invotomorph declaration; while theoretically
-- Isotype is recursive, as nested types are possible, it is currently implemented so as to not be
-- recursive, to simplify the logic of declaration parsing and validation.
data Isotype = Poly VName -- ^ Polymorphic typevars
             | Mono TName -- ^ Types of kind '*'
             | Bin TName VName {-Isotype-} -- ^ Types of kind '* -> *'
             | Tern TName VName VName {-Isotype Isotype -} -- ^ Types of kind '* -> * -> *'
             deriving (Data, Typeable, Show, Eq)

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

-- * Terms

-- | `Term` represents all possible left- and right-hand side expressions in the body of a morphism
-- declaration. Unlike `Isotype`, which does not support recursiveness for nested types, @Term@
-- supports recursion for nested expressions.
data Term = Constant Integer -- ^ Integer constants
          | StrConst String -- ^ String constants
          | Var VName -- ^ Named variables
          | Unary CName -- ^ ADTs and unary constructors
          | Binary CName Term -- ^ Constructors taking an argument
          | Ternary CName Term Term -- ^ Constructors taking two arguments
          deriving (Data, Typeable, Show, Eq)

instance Lift Term where
  lift (Constant i) = (ConE 'Constant $@) <$> lift i
  lift (StrConst i) = (ConE 'StrConst $@) <$> lift i
  lift (Var vn) = (ConE 'Var $@) <$> lift vn
  lift (Unary cn) = (ConE 'Unary $@) <$> lift cn
  lift (Binary cn vn) = (\c v -> ConE 'Binary $@ c $@ v) <$> lift cn <*> lift vn
  lift (Ternary cn xn yn) = (\c x y ->
    ConE 'Ternary $@ c $@ x $@ y) <$> lift cn <*> lift xn <*> lift yn

-- * Sides
-- | Simple definitions of the LHS and RHS of morphism bodies
-- While theoretically the mirrored declaration of any morphism is just as valid as the non-mirrored
-- version, the two are distinguished for convenience.

newtype LHS = LHS { getL :: Term }
    deriving (Data, Typeable, Show, Eq)
instance Lift LHS where
  lift (LHS t) = (ConE 'LHS $@) <$> lift t

newtype RHS = RHS { getR :: Term }
    deriving (Data, Typeable, Show, Eq)
instance Lift RHS where
  lift (RHS t) = (ConE 'RHS $@) <$> lift t
