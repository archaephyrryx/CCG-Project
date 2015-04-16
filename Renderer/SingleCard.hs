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

renderCard :: GCR
renderCard g@GenCard{..} = base (unravel name) $
  H.div $ do
    H.div ! A.class_ "card-imgs" $ cardImgs g
    H.div ! A.class_ "card-text" $ cardText g
    H.div ! A.class_ "card-info" $ cardInfo g

curls :: UniCard c => c -> [String]
curls = map <$> (++).("static/cards/"++).setnum <*> suf
    where
      suf = (<*>[".jpg"]).(id:).(consd (utype.=TMane) [('b':)] [])

cardImgs :: UCR
cardImgs = -- (table #+).map cimage.curls
           -- ((H.table $).sequence_).map cimage.curls

cimage :: String -> Rendered
cimage s = -- tr #: td #: a # set href s #: img #. "card" # set src s #+ []
           {- H.tr $ do
                H.td $ do
                  H.a ! A.href (toValue s) $ do
                    H.img ! A.class_ "card"
                          ! A.src (toValue s)
           -}

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

cardInfo :: UCR
cardInfo c =
    div #. "panel panel-default quick-facts" #+
      [ div #. "panel-heading" #: (h3 #. "panel-title" #$ string "Quick Facts")
      , div #. "panel-body" #: (ul #$ info c)
      ]

cardInfo :: GCR
cardInfo g@GenCard{..} =
  H.div ! A.class_ "panel panel-default quick-facts" $ do
    H.div ! A.class_ "panel-heading" $ do
      H.h3 ! A.class_ "panel-title" $ "Quick Facts"
    H.div ! A.class_ "panel-body" $ do
      H.ul $ info g

info :: UCR'
info c = let items = (maskfilter mask allitems) in morph $ map renderi items
  where
    mask :: [Int]
    mask = typemask . utype $ c
    maskfilter [] [] = []
    maskfilter (x:xs) (y:ys) = ([id,(y:)]!!x) $ maskfilter xs ys
    renderi :: (String, UCR) -> Rendered
    renderi (nam, ucr) = li #+ [b #$ string nam, ucr c]

info :: GenCard -> Html
info g@GenCard{..} = let items = (maskfilter mask allitems)
                     in mapM_ renderi items
  where
    renderi :: (String, GCR) -> Html
    renderi (nam, gcr) =
        H.li $ (H.b (toHtml nam)) <> (gcr g)

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
propLink s f x = 
    --{- Renderer-style
     (a #. s # set R.text (f x) #+ [])
    --}
    {- Pages-style
      propLink s f x = H.a ! A.href (toValue ("/card?"++s++"="++(f x))) $ (toHtml $  f x)
    --}


setLink :: UCR
setLink = propLink "set" (show.uset)

typeLink :: UCR
typeLink = propLink "type" (show.utype)

rarLink :: UCR
rarLink = propLink "rarity" (show.urar)

colLink :: UCR
colLink = propLink "color" (maybe "Wild" show . ucolor)

cardTraits :: UCR
cardTraits = (.) --(span #+) $ 
                 --((H.span $).sequence_) $
    (map keyToTrait.ukeywords)

keyToTrait :: Keyword -> Rendered
keyToTrait = (span #$) . string . unbrace . unravel
           --   H.span . toHtml . unbrace . unravel
