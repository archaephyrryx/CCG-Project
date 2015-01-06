{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    DeriveDataTypeable, GeneralizedNewtypeDeriving,
	RecordWildCards, TemplateHaskell, TypeFamilies,
	OverloadedStrings #-}
module Filter ( Filter
              , cardHandler
              , deckHandler
        ) where

---------------------------------------------
import qualified Data.Text as T
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Control.Applicative
import           Snap.Core (writeText)
import           Snap.Snaplet
import           Data.Maybe
import           Control.Monad
import           Snap.Snaplet.Heist (heistLocal, render)
import qualified Heist.Interpreted as I
import           Heist
---------------------------------------------
import           Application
--import           Prelude hiding (min, max)
import           Cards.Common (Color(..), CSet(..), CardType(..), Rarity(..))
import           Cards.Common.Abbrev
import           Cards.Common.Color (spect)


data Selection = Selection { colorFilter :: Color
{-                           , setFilter :: CSet
                           , typeFilter :: CardType
                           , rarityFilter :: Rarity -}
                           }
                deriving (Show)

-- data MinMax = MinMax { min :: Int, max :: Int } deriving (Show)


data Filter = CardFilter { select :: Selection
      {-                   , powRange :: MinMax
                         , costRange :: MinMax
                         , reqRange :: MinMax -}
                         }
            | DeckFilter { select :: Selection }
            deriving (Show)

selectForm :: (Monad m) => Form T.Text m Selection
selectForm = Selection
    <$> "colorFilter" .: choice (map (\x -> (x, (T.pack $ show x))) (Wild:spect)) Nothing
{-    <*> "setFilter" .: choice (map (\x -> let y = toEnum x in (y, (T.pack $ show y))) [0..4]) Nothing
    <*> "typeFilter" .: choice (map (\x -> let y = toEnum x in (y, (T.pack $ show y))) [0..5]) Nothing
    <*> "rarityFilter" .: choice (map (\x -> let y = toEnum x in (y, (T.pack $ show y))) [0..5]) Nothing
-}

{-
minmaxForm :: (Monad m) => Form T.Text m MinMax
minmaxForm = check minmaxErrMsg validRange $ MinMax
    <$> "min" .: stringRead "Not a number" (Just  0)
    <*> "max" .: stringRead "Not a number" (Just 99)
    where
        minmaxErrMsg :: T.Text
        minmaxErrMsg = "invalid: min>max"
        validRange :: MinMax -> Bool
        validRange mm = (max mm) >= (min mm)
-}
        
cardForm :: (Monad m) => Form T.Text m Filter
cardForm = CardFilter
    <$> "select" .: selectForm
{-    <*> "powRange" .: minmaxForm
    <*> "costRange" .: minmaxForm
    <*> "reqRange" .: minmaxForm
-}

deckForm :: (Monad m) => Form T.Text m Filter
deckForm = DeckFilter
    <$> "select" .: selectForm

cardHandler :: Handler App App ()
cardHandler = do
    (view, result) <- runForm "cardform" cardForm
    case result of
        Just x  -> heistLocal (bindCard x) $ render "card"
        Nothing -> heistLocal (bindDigestiveSplices view) $ render "card-form"
    where
        bindCard card = 
            I.bindSplice "card" (I.textSplice (T.pack $ show card))

deckHandler :: Handler App App ()
deckHandler = do
    (view, result) <- runForm "deckform" deckForm
    case result of
        Just x  -> heistLocal (bindDeck x) $ render "deck"
        Nothing -> heistLocal (bindDigestiveSplices view) $ render "deck-form"
    where
        bindDeck deck = 
            I.bindSplice "deck" (I.textSplice (T.pack $ show deck))
