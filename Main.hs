{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Happstack.Server
import Site (cardPage, deckPage, homePage)

main :: IO ()
main = simpleHTTP nullConf $ msum
    [ dir "card" $ cardPage
    , dir "deck" $ deckPage
    , dir "home" $ homePage
    , dir "res"  $ resServe
    , homePage
    ]
    where
        resServe = serveDirectory EnableBrowsing [] "res"
