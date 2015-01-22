{-# LANGUAGE DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, OverlappingInstances,
    IncoherentInstances, OverloadedStrings, RecordWildCards,
    ScopedTypeVariables, TemplateHaskell, TypeFamilies,
    TypeSynonymInstances #-} 

module Pages.Card where
-------------------------------------------------
import Control.Monad
import Control.Applicative
import Control.Monad.Identity       ( Identity(runIdentity) )
import Control.Applicative.Indexed  ( IndexedFunctor(..) , IndexedApplicative(..))
import Data.Char
import Data.Data		            ( Data, Typeable )
import Data.Maybe
import Data.Monoid
import Data.List
import Data.List.Split
import Text.Blaze ((!))
import Text.Blaze.Html5 (Html, toValue, ToValue, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Happstack.Server
import Text.Reform.Happstack
import Text.Reform.Blaze.Text
import Text.Reform
--------------------------------------------------
import Database
import Data.IxSet
import Data.Map                   ( Map )
import qualified Data.Map         as Map
import Data.Set                   ( Set )
import qualified Data.Set         as Set
import Control.Exception	        ( bracket )
import Control.Monad.Reader         ( ask )
import Control.Monad.State	        ( get, put )
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
import Reformation
import Pages.Common (template, base)
---------------------------------------------------

type SetNum = (CSet, Number)
type GCR = GenCard -> Html

instance Read SetNum where
    readsPrec = const readsSN

instance ToValue Power where
    toValue = toValue . show . val

instance ToValue Req where
    toValue = toValue . show . val

instance ToValue Cost where
    toValue = toValue . show . val


readsSN :: ReadS SetNum
readsSN x = let (s, n) = splitAt 2 x in [((readCS s, readN n),"")]

single :: String -> Html
single x = let (s,n) = read x :: SetNum
           in renderCard (fromJust.getOne $ (cardDB @= (s) @= (n)))

renderCard :: GCR
renderCard g@GenCard{..} = base (unravel name) $
  H.div $ do
    H.div ! A.class_ "card-imgs" $ cardImgs g
    H.div ! A.class_ "card-text" $ cardText g
    H.div ! A.class_ "card-info" $ cardInfo g

cardImgs :: GCR
cardImgs g@GenCard{..} = let urls = curls g in H.table $ (mapM_ cimage urls)

curls :: GenCard -> [String]
curls g@GenCard{..} = let sn = genset g in map (("/res/cards/"++sn)++) (suffixes ctype)
    where
      suffixes :: CardType -> [String]
      suffixes TMane = [".jpg","b.jpg"]
      suffixes _ = [".jpg"]

cimage :: String -> Html
cimage s = 
    H.tr $ do
      H.td $ do
        H.a ! A.href (toValue s) $ image
  where
    image = H.img ! A.class_ "card" ! A.src (toValue s)

cardText :: GCR
cardText g@GenCard{ctype=TMane, ..} =
  H.dl $ do
    H.div $ do
      H.dt $ "Card Text"
      H.dd $ do
        mapM_ (H.p.toHtml) $ (splitOn "<P>" (fromJust (stripPrefix "Front: " (head (splitOn " Back: " (unravel text))))))
    H.div $ do
      H.dt $ "Boosted Card Text"
      H.dd $ do
        mapM_ (H.p.toHtml) $ (splitOn "<P>" (last (splitOn " Back: " (unravel text))))

cardText g@GenCard{..} =
  H.dl $ do
    H.div $ do
      H.dt $ "Card Text"
      H.dd $ do
        mapM_ (H.p.toHtml) $ (splitOn "<P>" (unravel text))

cardInfo :: GCR
cardInfo g@GenCard{..} =
  H.div $ do
    H.div ! A.class_ "panel panel-default quick-facts" $ do
      H.div ! A.class_ "panel-heading" $ do
        H.h3 ! A.class_ "panel-title" $ "Quick Facts"
      H.div ! A.class_ "panel-body" $ do
        H.ul $ info g

info :: GenCard -> Html
info g@GenCard{..} = let items = (maskfilter mask allitems)
                     in mapM_ renderi items
  where
    mask :: [Bool]
    mask = typemask ctype
    maskfilter [] [] = []
    maskfilter (x:xs) (y:ys) = let ts = maskfilter xs ys in if x then y:ts else ts
    renderi :: (String, GCR) -> Html
    renderi (nam, gcr) =
        H.li $ (H.b (toHtml nam)) <> (gcr g)

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
propLink s f g =
  H.a ! A.href (toValue ("/card?"++s++"="++(f g))) $ (toHtml $  f g)

setLink :: GCR
setLink = propLink "set" (show.gset)
typeLink :: GCR
typeLink = propLink "type" (show.ctype)
rarLink :: GCR
rarLink = propLink "rarity" (show.grar)
colLink :: GCR
colLink = propLink "color" (maybe "Wild" show . mcolor)


reqtify :: GCR
reqtify g@GenCard{..} = cbox (show.val<$>mreq, mcolor)
empower :: GCR
empower g@GenCard{..} = cbox (show.val<$>mpower, mcolor)
appraise :: GCR
appraise g@GenCard{..} = cbox (show.val<$>mcost, Nothing)

conf :: GCR
conf g@GenCard{ctype=TProblem, ..} =
  H.span $ do
    mapM_ (\(y,x) -> cbox (pure.show.val $ x, pure $ y)).fst.fromJust $ mpreqs
conf _ = cbox (Nothing, Nothing)
conf' :: GCR
conf' g@GenCard{ctype=TProblem, ..} = (\x -> cbox (Just (show.val $ x), Nothing)).snd.fromJust $ mpreqs
conf' _ = cbox (Nothing, Nothing)

cardTraits :: GCR
cardTraits g@GenCard{..} = H.span $ mapM_ keyToTrait keywords

keyToTrait :: Keyword -> Html
keyToTrait = H.span . toHtml . unbrace . unravel

unbrace :: String -> String
unbrace [] = []
unbrace x | head x == '[' && last x == ']' = init.tail $ x
          | otherwise = x

cbox :: (Maybe String, Maybe Color) -> Html
cbox (Nothing,_) = H.span $ mempty
cbox (Just s, c) = H.span ! A.class_ (toValue (unwords ["element","label",(colorize c)])) $ (toHtml s)
  where
    colorize :: Maybe Color -> String
    colorize (Nothing) = "NoColor"
    colorize (Just Wild) = "Wild"
    colorize (Just c) = show c
    
pronounce :: (String, String) -> Html
pronounce (nam, code) = H.a ! A.href (toValue ("card/"++code)) $ (toHtml nam)
