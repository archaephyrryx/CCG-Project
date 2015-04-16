{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards,
   TupleSections, Rank2Types #-}

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
import qualified Data.Text          as Strict
import qualified Data.Text.Lazy     as Lazy
import Data.Text.Lazy               ( Text )
import Happstack.Server
import Happstack.Server.HSP.HTML
import Happstack.Server.XMLGenT
import HSP hiding (set)
import HSP.Monad                    ( HSPT(..) )
import Language.Haskell.HSX.QQ      ( hsx )

type UCR = UniCard c => Renderer c
type UCR' = UniCard c => Renderer' c

iconic :: Renderer CardType
iconic x = let ipath :: String
               ipath = ("static/icns/"++(show x)++".png")
           in (img #. "icon typeIcon" # set src ipath) #+ []

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
    colorize (Nothing) = "NoColor"
    colorize (Just Wild) = "Wild"
    colorize (Just c) = show c

conf :: UCR
conf = (span #+).map (cbox.zmap (pure,pure).(show.<val$<).swap).(fst?/).upreqs

conf' :: UCR
conf' = (span #+).(once (cbox.(pure.<show.<val$<).(,Nothing).snd)?/).upreqs
