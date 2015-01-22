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
import Pages.Card (single)
import Application
import Cards.Common
import Cards.Common.Abbrev
import Cards.Common.Hint

main :: IO ()
main = simpleHTTP nullConf $ msum
        [ dir "res"  $ resServe
        , handlebar
        ]
    where resServe = serveDirectory EnableBrowsing [] "res"

handlebar :: ServerPartT IO Response 
handlebar = do
    msum [ dir "card" $ msum
            [ nullDir >> do mMinPower <- (readH <$>) <$> (optional $ look "minPower")
                            mMaxPower <- (readH <$>) <$> (optional $ look "maxPower")
                            mMinCost  <- (readH <$>) <$> (optional $ look "minCost")
                            mMaxCost  <- (readH <$>) <$> (optional $ look "maxCost")
                            mMinReq   <- (readH <$>) <$> (optional $ look "minReq")
                            mMaxReq   <- (readH <$>) <$> (optional $ look "maxReq")
                            mcolors   <- (map readC <$>) <$> (optional $ looks "color")
                            msets     <- (map readCS <$>) <$> (optional $ looks "set")
                            mrars     <- (map long <$>) <$> (optional $ looks "rarity")
                            mtypes    <- (map readT <$>) <$> (optional $ looks "type")
                            ok $ card ([mMinPower,mMaxPower],[mMinCost,mMaxCost],[mMinReq,mMaxReq]) mcolors msets mrars mtypes
            , path $ \s -> ok $ single s
            ]
         , dir "deck" $
              nullDir >> do mcolors <- (map readC <$>) <$> (optional $ looks "color")
                            msets <- (map readCS <$>) <$> (optional $ looks "set")
                            mrars <- (map long <$>) <$> (optional $ looks "rarity")
                            mtypes <- (map readT <$>) <$> (optional $ looks "type")
                            ok $ deck mcolors msets mrars mtypes
         , dir "home" $ ok $ home
         , ok $ home
         ]
