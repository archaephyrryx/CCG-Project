{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Data.Dimorph.Test where

import Data.Dimorph.Quasi
import Data.Dimorph.Prim
import Data.Dimorph.Language
import Data.Dimorph.Parse

data X = A | C deriving (Show, Read, Eq)

data Y = B | D deriving (Show, Read, Eq)

$(quoteDiDec "iso X Y A <=> B C <=> D")
