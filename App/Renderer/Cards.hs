{-# LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances #-} 

module App.Renderer.Cards where
-------------------------------------------------
import Data.Maybe
import Data.List
--------------------------------------------------
import Cards.Generic
import Cards.Common
--------------------------------------------------
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
---------------------------------------------------

type GCR = GenCard -> UI Element
type GCR' = GenCard -> [UI Element]

iconic :: CardType -> UI Element
iconic x = let ipath = "res/icns/"++(show x)++".png" in
          UI.img #. "icon" # UI.set UI.src ipath

reqtify :: GCR
reqtify g@GenCard{..} = cbox (show.val<$>mreq, mcolor)

empower :: GCR
empower g@GenCard{..} = cbox (show.val<$>mpower, mcolor)

cbox :: (Maybe String, Maybe Color) -> UI Element
cbox (Nothing,_) = UI.span #+ []
cbox (Just s, c) = UI.span #. (unwords ["element","label",(colorize c)]) #+ [string s]
  where
    colorize :: Maybe Color -> String
    colorize (Nothing) = "NoColor"
    colorize (Just Wild) = "Wild"
    colorize (Just c) = show c
