{-# LANGUAGE RecordWildCards #-} 
module Renderer.Deck where
-------------------------------------------------
import Prelude hiding (div)
import Data.Maybe
import Data.List.Split
import Data.List
--------------------------------------------------
import API.Database
import Data.IxSet
--------------------------------------------------
import CCG
--------------------------------------------------
import Util
--------------------------------------------------
import Renderer.Core
import Renderer.Cards
import Renderer.SingleCard
--------------------------------------------------
import qualified Graphics.UI.Threepenny            as UI
import qualified Graphics.UI.Threepenny.Core       as UI
import qualified Graphics.UI.Threepenny.Elements   as UI
import qualified Graphics.UI.Threepenny.Attributes as UI
import Graphics.UI.Threepenny.Core hiding (get, set)
---------------------------------------------------

construct :: Deck -> Rendered'
construct d = [ UI.div #. "mane" #+ [
                  UI.div #. "mane-deck" #+ [
                    UI.h2 #. "mane-title" #+ [mheader nmane]]
                  , UI.hr
                  , UI.div #. "mane-cards" #+ [con.struct $ manes]
                ]
              , UI.div #. "problem" #+ [
                  UI.div #. "problem-deck" #+ [
                    UI.h2 #. "problem-title" #+ [pheader nprob start]]
                  , UI.hr
                  , UI.div #. "problem-cards" #+ [con.struct $ probs]
                ]
              , UI.div #. "draw" #+ [
                  UI.div #. "draw-deck" #+ [
                    UI.h2 #. "draw-title" #+ [dheader ndraw]]
                  , UI.hr
                  , UI.div #. "draw-cards" #+ [con.struct $ draws]
                ]
              ]
    where
      parts@(manes, probs, draws) = tpart d
      lens@(nmane,nprob,ndraw) = mhall (length,length,length) parts
      start = hasStarting probs

mheader :: Int -> UI Element
mheader n = UI.span #+ [string $ "Manes ("++(show n)++")"]

pheader :: Int -> Bool -> UI Element
pheader n s = UI.span #+ ((s?:(++[UI.span #. "no-start" #+ [string "No Starting Problem!"]])) [string $ "Problem Deck ("++(show n)++"/10)"])

dheader :: Int -> UI Element
dheader n = UI.span #+ [string $ "Draw Deck ("++(show n)++"/45)"]

con :: [(Card,Int)] -> UI Element
con xs = UI.div #. "card-box" #+ (map (\x -> UI.div #. "card-line" #+ [cline x]) xs)
  where
    cline (c,n) = UI.span #. "cline" #+ (account [ UI.span #. "ctab" #+ (ctab c) , UI.span #. "cname" #+ [string . name $ c]])
      where
        account = ((++[UI.span #. "ccount badge" #+ [string (show n)]])?+n)
        ctab :: Card -> [UI Element]
        ctab = cond ((==TProblem).cardtype) (conf.toGeneric) (empower.toGeneric)
          where
            empower g@GenCard{..} = map cbox . filter (isJust.fst) $ [(show.val<$>mpower, mcolor)]
            conf g@GenCard{ctype=TProblem, ..} = map (\(y,z) -> cbox (Just (show (val z)), Just y)).fst.fromJust$mpreqs
            conf _ = []
            cbox (Nothing,_) = UI.span #+ []
            cbox (Just s, c) = UI.span #. (unwords ["element","label",(colorize c)]) #+ [string s]
              where
                colorize (Nothing) = "NoColor"
                colorize (Just Wild) = "Wild"
                colorize (Just c) = show c
