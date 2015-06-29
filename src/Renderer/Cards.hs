{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Renderer.Cards where

---------------------------------------------
import qualified Data.Text as T
import           Control.Applicative
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Data.Maybe
import           Control.Monad
import           Heist
import qualified Heist.Interpreted as I
---------------------------------------------
import           Application
import           CCG hiding (render)
import           API.Filter
import           Renderer.Core

iconSplice :: CardType -> Splices (SnapletISplice App)
iconSplice x = do
    "typesource" ## string ipath
  where
      ipath :: String
      ipath = ("/res/icns/"++(show x)++".png")

cbox :: (Maybe String, Maybe Color) -> SnapletISplice App
cbox x = I.callTemplate "_cbox" (cboxSplice x)
  where
    cboxSplice (Nothing, _) = do
        "classes" ## string ""
        "number" ## string ""
    cboxSplice (Just s, c) = do
        "classes" ## string (unwords ["element","label",(colorize c)])
        "number" ## string s
      where
        colorize :: Maybe Color -> String
        colorize = maybe "NoColor" show

colored :: (Hint h, UniCard u) => (forall c. UniCard c => c -> Maybe h) -> u -> SnapletISplice App
colored f = cbox . ((,) <$> ((showH <$>) . f) <*> ucolor)

reqtify :: UniCard c => c -> SnapletISplice App
reqtify = colored ureq

empower :: UniCard c => c -> SnapletISplice App
empower = colored upower

appraise :: UniCard c => c -> SnapletISplice App
appraise = colored ucost
