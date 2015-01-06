{-# LANGUAGE OverloadedStrings #-}

module Main where

import Happstack.Server
import Control.Monad
import Text.Blaze ((!))
import Data.Monoid ((<>))
import Data.List (intercalate)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Site

main :: IO ()
main = simpleHTTP nullConf $ msum
    [ dir "card" $ ok $ card
    , dir "deck" $ ok $ deck
    , ok $ home
    ]
