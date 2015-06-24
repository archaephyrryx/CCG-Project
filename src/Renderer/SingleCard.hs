{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Renderer.SingleCard where
-------------------------------------------------
import Data.Maybe
import Data.List hiding (span)
import Data.List.Split
import Data.Char
import Data.Tuple
import Prelude hiding (span, div)
-------------------------------------------------
import Control.Applicative
import Control.Monad
--------------------------------------------------
import CCG hiding (set)
import API.Parser
--------------------------------------------------
import Util
--------------------------------------------------
import Renderer.Cards
import Renderer.Core hiding (text)
import qualified Renderer.Core as R (text)
--------------------------------------------------

renderCard :: UniCard c => c -> (Builder -> Rendered)
renderCard c =
  (#+ [ div #. "card-imgs" #: cardImgs c
      , div #. "card-text" #: cardText c
      ])

curls :: UniCard c => c -> [String]
curls = map <$> (++).("static/cards/"++).setnum <*> suf
    where
      suf = (<*>[".jpg"]).(id:).(consd (utype.=TMane) [('b':)] [])

cardImgs :: UCR
cardImgs = (table #+).map cimage.curls

cimage :: String -> Rendered
cimage s = tr #: td #: a # set href s #: img #. "card" # set src s #+ []

cardText :: UCR
cardText c = let m = maneText c
                 labs =((++) <$> ["","Boosted "] <*> ["Card Text"])
             in dl #$ (collect $ map (morph.dbox) $ zip m labs)
    where
      textBox :: [String] -> Rendered
      textBox = (div #+) . map ((p #$).string)
      maneText :: UniCard c => c -> [Rendered]
      maneText = map textBox . pbreaks . parseTexts
      dbox :: (Rendered, String) -> [Rendered]
      dbox (disp,lab) = [ dt #$ string lab, dd #: disp ]

cardInfo :: UCR
cardInfo c =
    div #. "panel panel-default quick-facts" #+
      [ div #. "panel-heading" #: (h3 #. "panel-title" #$ string "Quick Facts")
      , div #. "panel-body" #: (ul #$ info c)
      ]

info :: UCR'
info c = let items = (maskfilter mask allitems) in morph $ map renderi items
  where
    mask :: [Int]
    mask = typemask . utype $ c
    maskfilter [] [] = []
    maskfilter (x:xs) (y:ys) = ([id,(y:)]!!x) $ maskfilter xs ys
    renderi :: (String, UCR) -> Rendered
    renderi (nam, ucr) = li #+ [b #$ string nam, ucr c]

allitems :: [(String, UCR)]
allitems = [ ("Type", typeLink)
           , ("Rarity", rarLink)
           , ("Color", colLink)
           , ("Power", empower)
           , ("Cost", appraise)
           , ("Req", reqtify)
           , ("Confront", conf)
           , ("Opponent Confront", conf')
           , ("Traits", cardTraits)
           ]

typemask :: CardType -> [Int]
typemask x = case x of
    TMane         -> [1,1,1,1,0,0,0,0,1]
    TFriend       -> [1,1,1,1,1,1,0,0,1]
    TEvent        -> [1,1,1,1,1,1,0,0,1]
    TResource     -> [1,1,1,1,1,1,0,0,1]
    TTroublemaker -> [1,1,0,1,0,0,0,0,1]
    TProblem      -> [1,1,0,0,0,0,1,1,0]

propLink :: String -> (forall c. UniCard c => c -> String) -> (UCR)
propLink s f x = (a #. s # set R.text (f x) #+ [])

setLink, typeLink, rarLink, colLink :: UCR

setLink  = propLink "set" (show.uset)
typeLink = propLink "type" (show.utype)
rarLink  = propLink "rarity" (show.urar)
colLink  = propLink "color" (maybe "Wild" show . ucolor)

cardTraits :: UCR
cardTraits = (.) (span #+) $ (map keyToTrait.ukeywords)

keyToTrait :: Keyword -> Rendered
keyToTrait = (span #$) . string . unbrace . unravel
