{-# LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances #-} 

module Renderer.Cards where
-------------------------------------------------
import Data.Maybe
import Data.List
--------------------------------------------------
import CCG.Cards.Generic
import CCG.Cards.Common
--------------------------------------------------
import Renderer.Core
--------------------------------------------------

type GCR = GenCard -> Rendered
type GCR' = GenCard -> Rendered'

iconic :: CardType -> Rendered
iconic x = let ipath = "static/icns/"++(show x)++".png" in
          UI.img #. "icon typeIcon" # UI.set UI.src ipath

reqtify :: GCR
reqtify g@GenCard{..} = cbox (show.val<$>mreq, mcolor)

empower :: GCR
empower g@GenCard{..} = cbox (show.val<$>mpower, mcolor)

cbox :: (Maybe String, Maybe Color) -> Rendered
cbox (Nothing,_) = span #+ []
cbox (Just s, c) = span #. (unwords ["element","label",(colorize c)]) #+ [string s]
  where
    colorize :: Maybe Color -> String
    colorize (Nothing) = "NoColor"
    colorize (Just Wild) = "Wild"
    colorize (Just c) = show c
