{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Data.Invotomorph.Test where

import Data.Invotomorph.Quasi
import Data.Invotomorph.Prim
import Data.Invotomorph.Language
import Data.Invotomorph.Parse

data X = A | NotA
       | B | NotB
       | C | NotC
       deriving (Show, Read, Eq)

[inv|
  auto X
    A <-> NotA
    B <-> NotB
    C <-> NotC
  |]
