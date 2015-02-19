{-# LANGUAGE RecordWildCards, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module App.Renderer.SingleCard where
-------------------------------------------------
import Data.Maybe
import Data.List
import Data.List.Split
-------------------------------------------------
import Control.Applicative
import Control.Monad
--------------------------------------------------
import Cards
import Cards.Common
import Cards.Differentiation
import Cards.Generic
--------------------------------------------------
import App.Renderer.Cards
import App.Core.Helper
--------------------------------------------------
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core

renderCard :: GCR'
renderCard g = [ UI.div #. "card-imgs" #+ [cardImgs g]
               , UI.div #. "card-text" #+ [cardText g]
               , UI.div #. "card-info" #+ [cardInfo g]
               ]

cardImgs :: GCR
cardImgs = (UI.table #+).(map cimage.curls)

curls :: GenCard -> [String]
curls g@GenCard{..} = let sn = genset g in map (("/res/cards/"++sn)++) (suffixes ctype)
    where
      suffixes :: CardType -> [String]
      suffixes x = id:boost x <*> [suffix]
      boost TMane = [('b':)]
      boost _ = []
      suffix = ".jpg"

cimage :: String -> UI Element
cimage s = UI.tr #+ [UI.td #+ [UI.a # UI.set (attr "href") s #+ [UI.img #. "card" # UI.set (attr "src") s]]]

cardText :: GCR
cardText g = let m = maneText g
                 labs =((++) <$> ["","Boosted "] <*> ["Card Text"])
             in UI.dlist #+ (map dbox $ zip m labs)

dbox :: ([UI Element], String) -> UI Element
dbox (disp,lab) = UI.div #+ [ UI.dterm #+ [string lab]
                            , UI.ddef #+ disp]

maneText :: GenCard -> [[UI Element]]
maneText g@GenCard{..} = case ctype of
    TMane -> let (('F':'r':'o':'n':'t':':':' ':front):back:[]) = splitOn " Back: " (unravel text)
             in map textBox [front,back]
    _ -> [textBox (unravel text)]

textBox :: String -> [UI Element]
textBox = map ((UI.p #+).(:[]).string) . splitOn "<P>"

cardInfo :: GCR
cardInfo g@GenCard{..} = UI.div #. "panel panel-default quick-facts" #+ [
                            UI.div #. "panel-heading" #+ [UI.h3 #. "panel-title" #+ [string "Quick Facts"]]
                          , UI.div #. "panel-body" #+ [UI.ul #+ info g]
                          ]

info :: GenCard -> [UI Element]
info g@GenCard{..} = let items = (maskfilter mask allitems) in map renderi items
  where
    mask :: [Int]
    mask = typemask ctype
    renderi :: (String, GCR) -> UI Element
    renderi (nam, gcr) = UI.li #+ [UI.bold #+ [string nam], gcr g]
    maskfilter [] [] = []
    maskfilter (x:xs) (y:ys) = ([id,(y:)]!!x) $ maskfilter xs ys

typemask :: CardType -> [Int]
typemask x = case x of
    TMane         -> [1,1,1,1,0,0,0,0,1]
    TFriend       -> [1,1,1,1,1,1,0,0,1]
    TEvent        -> [1,1,1,1,1,1,0,0,1]
    TResource     -> [1,1,1,1,1,1,0,0,1]
    TTroublemaker -> [1,1,0,1,0,0,0,0,1]
    TProblem      -> [1,1,0,0,0,0,1,1,0]

allitems :: [(String, GCR)]
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

propLink :: String -> (GenCard -> String) -> (GCR)
propLink s f g = UI.a #. s # UI.set UI.text (f g)

setLink :: GCR
setLink = propLink "set" (show.gset)
typeLink :: GCR
typeLink = propLink "type" (show.ctype)
rarLink :: GCR
rarLink = propLink "rarity" (show.grar)
colLink :: GCR
colLink = propLink "color" (maybe "Wild" show . mcolor)

appraise :: GCR
appraise g@GenCard{..} = cbox (show.val<$>mcost, Nothing)

conf :: GCR
conf g@GenCard{ctype=TProblem, ..} = UI.span #+ (map (\(y,x) -> cbox (pure.show.val $ x, pure $ y)).fst.fromJust $ mpreqs )
conf _ = cbox (Nothing, Nothing)

conf' :: GCR
conf' g@GenCard{ctype=TProblem, ..} = (\x -> cbox (pure.show.val $ x, Nothing)).snd.fromJust $ mpreqs
conf' _ = cbox (Nothing, Nothing)

cardTraits :: GCR
cardTraits g@GenCard{..} = UI.span #+ (map keyToTrait keywords)

keyToTrait :: Keyword -> UI Element
keyToTrait k = UI.span #+ [string (unbrace (unravel k))]

unbrace :: String -> String
unbrace [] = []
unbrace x | head x == '[' && last x == ']' = init.tail $ x
          | otherwise = x
