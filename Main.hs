{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Applicative
import Happstack.Server
import Site
import Pages.Card (single)
import Application
import CCG

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
            [ nullDir >> do cFilter <- parseCardFilter
            card ([mMinPower,mMaxPower],[mMinCost,mMaxCost],[mMinReq,mMaxReq]) mcolors msets mrars mtypes
            , path $ \s -> single s
            ]
         , dir "deck" $
              nullDir >> do dFilter < parseDeckFilter
         , dir "home" $ home
         , home
         ]

parseCardFilter = do
    mMinPower <- (readH <$>) <$> (optional $ look "minPower")
    mMaxPower <- (readH <$>) <$> (optional $ look "maxPower")
    mMinCost  <- (readH <$>) <$> (optional $ look "minCost")
    mMaxCost  <- (readH <$>) <$> (optional $ look "maxCost")
    mMinReq   <- (readH <$>) <$> (optional $ look "minReq")
    mMaxReq   <- (readH <$>) <$> (optional $ look "maxReq")
    mcolors   <- (map readC <$>) <$> (optional $ looks "color")
    msets     <- (map readCS <$>) <$> (optional $ looks "set")
    mrars     <- (map long <$>) <$> (optional $ looks "rarity")
    mtypes    <- (map readT <$>) <$> (optional $ looks "type")

parseDeckFilter = do
    mcolors <- (map readC <$>) <$> (optional $ looks "color")
    msets <- (map readCS <$>) <$> (optional $ looks "set")
    mrars <- (map long <$>) <$> (optional $ looks "rarity")
    mtypes <- (map readT <$>) <$> (optional $ looks "type")
