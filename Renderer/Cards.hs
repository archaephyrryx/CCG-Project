{-# LANGUAGE TupleSections, Rank2Types #-}

module Renderer.Cards where
-------------------------------------------------
import Data.Maybe
import Data.List hiding (span)
import Control.Applicative
import Prelude hiding (span)
--------------------------------------------------
import CCG hiding (set)
import Util
--------------------------------------------------
import Renderer.Core
--------------------------------------------------

type UCR = UniCard c => Renderer c
type UCR' = UniCard c => Renderer' c

iconic :: Renderer CardType
iconic x = img #. "icon typeIcon" # set src ipath # act
  where
      ipath :: String
      ipath = ("static/icns/"++(show x)++".png")

colored :: Hint h => (forall c. UniCard c => c -> Maybe h) -> UCR
colored f = cbox . ((,) <$> ((showH <$>) . f) <*> ucolor)

reqtify :: UCR
reqtify = colored ureq

empower :: UCR
empower = colored upower

appraise :: UCR
appraise = colored ucost

cbox :: Renderer (Maybe String, Maybe Color)
cbox (Nothing,_) = span #+ []
cbox (Just s, c) = span #. (unwords ["element","label",(colorize c)]) #$ string s
  where
    colorize :: Maybe Color -> String
    colorize = maybe "NoColor" show

conf :: UCR
conf = (span #+).map (cbox.resp (pure,pure).(show.val$<).swap).(fst?/).upreqs

conf' :: UCR
conf' = (span #+).(once (cbox.(pure.show.val$<).(,Nothing).snd)?/).upreqs
