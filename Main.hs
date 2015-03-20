{-# LANGUAGE OverloadedStrings #-}

module Main where

import Happstack.Server
import Control.Monad
import Control.Applicative
import Text.Blaze ((!))
import Data.Monoid ((<>))
import Data.List (intercalate)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Site
import Pages.Card (single)
import CCG

main :: IO ()
main = simpleHTTP nullConf $ msum
        [ dir "res"  $ resServe
        , dir "card" $ path $ page.single
        , dir "home" $ page home
        , handlebar
        , page home
        ]
    where resServe = serveDirectory EnableBrowsing [] "res"

handlebar :: ServerPartT IO Response 
handlebar = do
    decodeBody (defaultBodyPolicy "/tmp" 0 10000 10000)
    msum [ dir "card" $
              nullDir >> do mMinPower <- (readH <$>) <$> (optional $ look "minPower")
                            mMaxPower <- (readH <$>) <$> (optional $ look "maxPower")
                            mMinCost  <- (readH <$>) <$> (optional $ look "minCost")
                            mMaxCost  <- (readH <$>) <$> (optional $ look "maxCost")
                            mMinReq   <- (readH <$>) <$> (optional $ look "minReq")
                            mMaxReq   <- (readH <$>) <$> (optional $ look "maxReq")
                            mcolors   <- (map readC <$>) <$> (optional $ looks "color")
                            msets     <- (map readCS <$>) <$> (optional $ looks "set")
                            mrars     <- (map long <$>) <$> (optional $ looks "rarity")
                            mtypes    <- (map readT <$>) <$> (optional $ looks "type")
                            card ([mMinPower,mMaxPower],[mMinCost,mMaxCost],[mMinReq,mMaxReq]) mcolors msets mrars mtypes
         , dir "deck" $
              nullDir >> do mcolors <- (map readC <$>) <$> (optional $ looks "color")
                            msets <- (map readCS <$>) <$> (optional $ looks "set")
                            mrars <- (map long <$>) <$> (optional $ looks "rarity")
                            mtypes <- (map readT <$>) <$> (optional $ looks "type")
                            deck mcolors msets mrars mtypes
         ]
