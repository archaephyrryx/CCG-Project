{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Applicative
import Happstack.Server
import Site
import Pages.Card (single)
import Application
import CCG
import Query

main :: IO ()
main = simpleHTTP nullConf $ msum
        [ dir "res"  $ resServe
        , handlebar
        ]
    where resServe = serveDirectory EnableBrowsing [] "res"

handlebar :: ServerPartT IO Response 
handlebar = page $ do
    decodeBody (defaultBodyPolicy "/tmp" 0 10000 10000)
    msum [ dir "card" $ msum
            [ nullDir >> parseCardFilter >>= card
            , path $ \s -> single s
            ]
         , dir "deck" $
              nullDir >> parseDeckFilter >>= deck
         , dir "home" $ home
         , home
         ]
