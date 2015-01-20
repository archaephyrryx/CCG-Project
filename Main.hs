{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Applicative
import Happstack.Server
import Site
import Pages.Card (single)
import Application
import Cards.Common
import Cards.Common.Abbrev

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
            [ nullDir >> do mcolors <- (map readC <$>) <$> (optional $ looks "color")
                            msets <- (map readCS <$>) <$> (optional $ looks "set")
                            mrars <- (map long <$>) <$> (optional $ looks "rarity")
                            mtypes <- (map readT <$>) <$> (optional $ looks "type")
                            card mcolors msets mrars mtypes
            , path $ \s -> single s
            ]
         , dir "deck" $
              nullDir >> do mcolors <- (map readC <$>) <$> (optional $ looks "color")
                            msets <- (map readCS <$>) <$> (optional $ looks "set")
                            mrars <- (map long <$>) <$> (optional $ looks "rarity")
                            mtypes <- (map readT <$>) <$> (optional $ looks "type")
                            deck mcolors msets mrars mtypes
         , dir "home" $ home
         , home
         ]
