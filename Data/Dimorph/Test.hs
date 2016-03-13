{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiParamTypeClasses #-}
module Data.Dimorph.Test where

import Data.Dimorph.Quasi
import Data.Dimorph.Prim
import Data.Dimorph.Language
import Data.Dimorph.Parse
import Data.Dimorph.Alt

data X = A | C deriving (Show, Read, Eq)

data Y = B | D deriving (Show, Read, Eq)

[dimorph|iso X Y A <=> B C <=> D|]
