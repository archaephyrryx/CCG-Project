{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE EmptyCase #-}
module Data.Dimorph.Test where

import Data.Dimorph.Quasi
import Data.Dimorph.Prim
import Data.Dimorph.Language
import Data.Dimorph.Parse
import Data.Dimorph.Alt
import Data.Dimorph.Derive
import Data.Dimorph.X

[dimorph|
iso X Integer
A <=> 1
C <=> 2
E True <=> 3
E False <=> 4
|]
