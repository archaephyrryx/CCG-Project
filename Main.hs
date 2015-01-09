{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Happstack.Server
import Site (card, cardPage, deck, deckPage, home, homePage, page, paginate)
import Application

main :: IO ()
main = simpleHTTP nullConf $ msum
        [ dir "res"  $ resServe
        , handlebar
        ]
    where
        resServe = serveDirectory EnableBrowsing [] "res"

handlebar :: ServerPartT IO Response 
handlebar = page . paginate $ do
    decodeBody (defaultBodyPolicy "/tmp" 0 10000 10000)
    msum [ card
         , deck
         , dir "home" $ home
         , home
         ]
