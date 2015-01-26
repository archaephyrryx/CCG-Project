{-# LANGUAGE RecordWildCards #-} 

module CCGServer.Card where
-------------------------------------------------
import Data.Maybe
--import Data.List.Split
--import Data.List
--------------------------------------------------
--import Database
--import Data.IxSet
--------------------------------------------------
--import Cards hiding (set)
import Cards.Generic hiding (set)
import Cards.Common
--import Cards.Common.Color
--import Cards.Common.Stringe
--import Cards.Common.Hint
--import Cards.Common.Abbrev
--import Cards.Common.Values
--import Cards.Differentiation
--import MLPCCG
--------------------------------------------------
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
---------------------------------------------------

{-
type SetNum = (CSet, Number)
type GCR = GenCard -> UI Element

instance Read SetNum where
    readsPrec = const readsSN

readsSN :: ReadS SetNum
readsSN x = let (s, n) = splitAt 2 x in [((readCS s, readN n),"")]

iconic :: GCR
iconic x = let ipath = "res/icns/"++(show x)++".png" in
          UI.img #. "icon" # set UI.src ipath
-}
{-
single :: GCR
single x = let (s,n) = read x :: SetNum
           in renderCard (head.toList $ (cardDB @= (s) @= (n)))
-}


reqtify :: GenCard -> UI Element
reqtify g@GenCard{..} = cbox (show.val <$> (mreq), mcolor)
{-
empower :: GCR
empower g@GenCard{..} = cbox (show.val<$>mpower, mcolor)
appraise :: GCR
appraise g@GenCard{..} = cbox (show.val<$>mcost, Nothing)

conf :: GCR
conf g@GenCard{ctype=TProblem, ..} = UI.span #+ (map (\(y,x) -> cbox (pure.show.val $ x, pure $ y)).fst.fromJust $ mpreqs )
conf _ = cbox (Nothing, Nothing)
conf' :: GCR
conf' g@GenCard{ctype=TProblem, ..} = (\x -> cbox (Just (show.val $ x), Nothing)).snd.fromJust $ mpreqs
conf' _ = cbox (Nothing, Nothing)

cardTraits :: GCR
cardTraits g@GenCard{..} = UI.span #+ (map keyToTrait keywords)

keyToTrait :: Keyword -> UI Element
keyToTrait k = UI.span #+ [string (unbrace (unravel k))]

unbrace :: String -> String
unbrace [] = []
unbrace x | head x == '[' && last x == ']' = init.tail $ x
          | otherwise = x

-}
cbox :: (Maybe String, Maybe Color) -> UI Element
cbox _ = UI.span #+ []
{-
cbox (Nothing,_) = UI.span #+ []
cbox (Just s, c) = UI.span #. (unwords ["element","label",(colorize c)]) #+ [string s]
  where
    colorize :: Maybe Color -> String
    colorize (Nothing) = "NoColor"
    colorize (Just Wild) = "Wild"
    colorize (Just c) = show c
    
pronounce :: (String, String) -> UI Element
pronounce (nam, code) = UI.a #. "single-card" # set (attr "href") ("card/"++code) #+ [string nam]

renderCard :: GCR
renderCard g@GenCard{..} = UI.div #+ [ UI.div #. "card-imgs" #+ [cardImgs g]
                                     , UI.div #. "card-text" #+ [cardText g]
                                     , UI.div #. "card-info" #+ [cardInfo g]
                                     ]
cardImgs :: GCR
cardImgs g@GenCard{..} = let urls = curls g in UI.table #+ map cimage urls

curls :: GenCard -> [String]
curls g@GenCard{..} = let sn = genset g in map (("/res/cards/"++sn)++) (suffixes ctype)
    where
      suffixes :: CardType -> [String]
      suffixes TMane = [".jpg","b.jpg"]
      suffixes _ = [".jpg"]

cimage :: String -> UI Element
cimage s = UI.tr #+ [UI.td #+ [UI.a # set href s #+ [UI.img #. "card" # set (attr "src") s]]]

cardText :: GCR
cardText g@GenCard{ctype=TMane, ..} = UI.dlist #+ [ UI.div #+ [ UI.dterm #+ [string "Card Text"]
                                                           , UI.ddef #+ front
                                                           ]
                                               , UI.div #+ [ UI.dterm #+ [string "Boosted Card Text"]
                                                           , UI.ddef #+ back
                                                           ]
                                               ]
    where
    front = brunlines (splitOn "<P>" (fromJust (stripPrefix "Front: " (head (splitOn " Back: " (unravel text))))))
    back = brunlines (splitOn "<P>" (last (splitOn " Back: " (unravel text))))
cardText g@GenCard{..} = UI.dlist #+ [UI.div #+ [UI.dterm #+ [string "Card Text"], UI.ddef #+ cardtext]]
    where
    cardtext = brunlines (splitOn "<P>" (unravel text))

brunlines :: [String] -> [UI Element]
brunlines xs = map brline xs
    where
      brline :: String -> UI Element
      brline s = UI.p #+ [string s]

cardInfo :: GCR
cardInfo g@GenCard{..} = UI.div #+ [ UI.div #. "panel panel-default quick-facts" #+ [ UI.div #. "panel-heading" #+ [UI.h3 #. "panel-title" #+ [string "Quick Facts"]], UI.div #. "panel-body" #+ [UI.ul #+ info g]]]

info :: GenCard -> [UI Element]
info g@GenCard{..} = let items = (maskfilter mask allitems) in map renderi items
  where
    mask :: [Bool]
    mask = typemask ctype
    renderi :: (String, GCR) -> UI Element
    renderi (nam, gcr) = UI.li #+ [UI.b #+ [string nam], gcr g]
    maskfilter [] [] = []
    maskfilter (x:xs) (y:ys) = let ts = maskfilter xs ys in if x then y:ts else ts

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

typemask :: CardType -> [Bool]
typemask x = map (==1) $ case x of
    TMane         -> [1,1,1,1,0,0,0,0,1]
    TFriend       -> [1,1,1,1,1,1,0,0,1]
    TEvent        -> [1,1,1,1,1,1,0,0,1]
    TResource     -> [1,1,1,1,1,1,0,0,1]
    TTroublemaker -> [1,1,0,1,0,0,0,0,1]
    TProblem      -> [1,1,0,0,0,0,1,1,0]

propLink :: String -> (GenCard -> String) -> (GCR)
propLink s f g = UI.a #. s # set UI.text (f g)

setLink :: GCR
setLink = propLink "set" (show.gset)
typeLink :: GCR
typeLink = propLink "type" (show.ctype)
rarLink :: GCR
rarLink = propLink "rarity" (show.grar)
colLink :: GCR
colLink = propLink "color" (maybe "Wild" show . mcolor)
-}
