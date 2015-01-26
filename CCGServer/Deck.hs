{-# LANGUAGE DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, OverlappingInstances,
    IncoherentInstances,
    OverloadedStrings, QuasiQuotes, RecordWildCards,
    ScopedTypeVariables, TemplateHaskell, TypeFamilies,
    TypeSynonymInstances #-} 
module CCGServer.Deck where
-------------------------------------------------
import Prelude hiding (div)
import Data.Maybe
import Data.List.Split
import Data.List
--------------------------------------------------
import Database
import Data.IxSet
--------------------------------------------------
import Cards
import Cards.Generic
import Cards.Common
import Cards.Common.Color
import Cards.Common.Stringe
import Cards.Common.Hint
import Cards.Common.Abbrev
import Cards.Common.Values
import Cards.Differentiation
import MLPCCG
--------------------------------------------------
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.UI Elements hiding (map)
import Graphics.UI.Threepenny.Core
---------------------------------------------------

infix 9 ?<
infix 9 ?+
infix 9 ?:
infixr 5 +++

(?<) :: (a -> a) -> Int -> (a -> a)
f?<0 = f
f?<_ = id

(?+) :: (a -> a) -> Int -> (a -> a)
f?+1 = id
f?+_ = f

(?:) :: Bool -> (a -> a) -> (a -> a)
p?:f = if p then f else id


type Deck = [Card]
type DeckP = [Card]

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> (a -> b)
cond p f g = \x -> if p x then f x else g x

mhall :: ((x -> x),(y -> y),(z -> z)) -> (x,y,z) -> (x,y,z)
mhall (f,g,h) (a,b,c) = (f a, g b, h c)

construct :: Deck -> UI Element
construct d = div #. "decktab" #+ [
                div #. "mane" #+ [
                  div #. "mane-deck" #+ [
                    h2 #. "mane-title" #+ [mheader nmane]]
                  , hr
                  , div #. "mane-cards" #+ [con.struct $ manes]
                ]
              , div #. "problem" #+ [
                  div #. "problem-deck" #+ [
                    h2 #+ [pheader nprob start]]
                  , hr
                  , div #. "problem-cards" #+ [con.struct $ probs]
                ]
              , div #. "draw" #+ [
                  div #. "draw-deck" #+ [
                    h2 #. "draw-title" #+ [dheader ndraw]]
                  , hr
                  , div #. "draw-cards" [con.struct $ draws]
                ]
              ]
    where
      parts@(manes, probs, draws) = tpart d
      lens@(nmane,nprob,ndraw) = mhall (length,length,length) parts
      start = hasStarting probs

hasStarting :: DeckP -> Bool
hasStarting = any (isPrefixOf "Starting Problem" . unravel . ctext)

tpart :: Deck -> (DeckP, DeckP, DeckP)
tpart d = foldr tpartition ([],[],[]) d

tpartition :: Card -> (DeckP, DeckP, DeckP) -> (DeckP, DeckP, DeckP)
tpartition c@Mane{..} = mhall ((c:),id,id)
tpartition c@Problem{..} = mhall (id,(c:),id)
tpartition c = mhall (id,id,(c:))

(+++) :: ([a],[b],[c]) -> ([a],[b],[c]) -> ([a],[b],[c])
(d,e,f)+++(g,h,i) = (d++g,e++h,f++i)

mheader :: Int -> UI Element
mheader n = UI.span #+ [string $ "Manes ("++(show n)++")"]

pheader :: Int -> Bool -> UI Element
pheader n s = UI.span #+ ((s?:(++[string #. "no-start" #+ [string "No Starting Problem!"]])) [string $ "Problem Deck ("++(show n)++"/10)"])

dheader :: Int -> UI Element
dheader n = UI.span #+ [string "Draw Deck ("++(show n)++"/45)"]

struct :: DeckP -> [(Card,Int)]
-- list-specific implementation
struct dp = let g = group . sort $ dp
                l = map length g
                u = map head g
            in zip u g

con :: [(Card,Int)] -> UI Element
con xs = div #. "card-box" (map (\x -> div #. "card-line" #+ [cline x]) xs)
  where
    cline (c,n) = UI.span #. "cline" #+ (((++[UI.span #. "ccount badge" #+ [string (show n)]])?+n) $ [UI.span #. "ctab" #+ (ctab c), UI.span #. "cname" #+ [string . cname $ c]])
      where
        ctab = (if (ctype c == TProblem) then (conf) else (empower)) . toGeneric
          where
            empower g@GenCard{..} = map cbox . filter (isJust.fst) $ [(show.val<$>mpower, mcolor)]
            conf g@GenCard{ctype=TProblem, ..} = map (\(y,x) -> cbox (pure.show.val$x,pure$y)).fst.fromJust$mpreqs
            conf _ = []
            cbox (Nothing,_) = UI.span #+ []
            cbox (Just s, c) = UI.span #. (unwords ["element","label",(colorize c)]) #+ [string s]
              where
                colorize (Nothing) = "NoColor"
                colorize (Just Wild) = "Wild"
                colorize (Just c) = show c
