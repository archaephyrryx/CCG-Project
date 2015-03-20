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
import Graphics.UI.Threepenny.Core ((#.),(#+))
---------------------------------------------------

construct :: Deck -> Rendered'
construct d = [ div #. "mane" #+ [
                  div #. "mane-deck" #+ [
                    h2 #. "mane-title" #+ [mheader nmane]]
                  , hr
                  , div #. "mane-cards" #+ [con.struct $ manes]
                ]
              , div #. "problem" #+ [
                  div #. "problem-deck" #+ [
                    h2 #. "problem-title" #+ [pheader nprob start]]
                  , hr
                  , div #. "problem-cards" #+ [con.struct $ probs]
                ]
              , div #. "draw" #+ [
                  div #. "draw-deck" #+ [
                    h2 #. "draw-title" #+ [dheader ndraw]]
                  , hr
                  , div #. "draw-cards" #+ [con.struct $ draws]
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
con xs = div #. "card-box" #+ (map (\x -> div #. "card-line" #+ [cline x]) xs)
  where
    cline (c,n) = UI.span #. "cline" #+ (account [ UI.span #. "ctab" #+ (ctab c) , UI.span #. "cname" #+ [string . cname $ c]])
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
