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

iconic :: CardType -> Html
iconic x = let ipath = "res/icns/"++(show x)++".png" in
  H.img ! A.class_ "icon"
        ! A.src (toValue ipath)


conf :: UCR
conf = (span #+).map (cbox.zmap (pure,pure).(show.<val$<).swap).(fst?/).upreqs

conf :: GCR
conf g@GenCard{ctype=TProblem, ..} =
  H.span $ do
    mapM_ (\(y,x) -> cbox (pure.show.val $ x, pure $ y)).fst.fromJust $ mpreqs
conf _ = cbox (Nothing, Nothing)

conf' :: UCR
conf' = (span #+).(once (cbox.(pure.<show.<val$<).(,Nothing).snd)?/).upreqs

conf' :: GCR
conf' g@GenCard{ctype=TProblem, ..} = (\x -> cbox (Just (show.val $ x), Nothing)).snd.fromJust $ mpreqs
conf' _ = cbox (Nothing, Nothing)


cbox :: Renderer (Maybe String, Maybe Color)
cbox (Nothing,_) = span #+ []
cbox (Just s, c) = span #. (unwords ["element","label",(colorize c)]) #$ string s
  where
    colorize :: Maybe Color -> String
    colorize = maybe "NoColor" show

cbox :: (Maybe String, Maybe Color) -> Html
cbox (Nothing,_) = H.span $ mempty
cbox (Just s, c) = H.span ! A.class_ (toValue (unwords ["element","label",(colorize c)])) $ (toHtml s)
  where
    colorize :: Maybe Color -> String
    colorize = maybe "NoColor" show

colored :: Hint h => (forall c. UniCard c => c -> Maybe h) -> UCR
colored f = cbox . ((,) <$> ((showH <$>) . f) <*> ucolor)

reqtify :: UCR
reqtify = colored ureq

empower :: UCR
empower = colored upower

appraise :: UCR
appraise = colored ucost
