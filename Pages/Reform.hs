{-# LANGUAGE DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, OverlappingInstances,
    IncoherentInstances, OverloadedStrings, RecordWildCards,
    ScopedTypeVariables, TemplateHaskell, TypeFamilies,
    TypeSynonymInstances #-} 

module Pages.Reform where

-------------------------------------------------
import Control.Applicative
import Control.Applicative.Indexed  ( IndexedFunctor(..) , IndexedApplicative(..))
import Control.Monad
import Control.Monad.Identity       ( Identity(runIdentity) )
import Data.Char
import Data.Data		            ( Data, Typeable )
import Data.Maybe
import Data.Monoid
import Text.Blaze ((!))
import Text.Blaze.Html5 (Html, ToMarkup, toHtml, toMarkup, toValue)
import Text.Blaze.Internal (MarkupM)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.List
import Data.Char
import Happstack.Server
import Text.Reform.Happstack
import Text.Reform.Blaze.String
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
import Data.List                    hiding (insert)
--------------------------------------------------
import Cards
import Cards.Generic
import Cards.Common
import Cards.Common.Color
import Cards.Common.Stringe
import Cards.Common.Hint
import Cards.Common.Abbrev
import Cards.Common.Values
import MLPCCG
--------------------------------------------------
import Reformation
import Pages.Common
import Pages.Card
---------------------------------------------------

newtype AppError = AppCFE (CommonFormError [Input])
    deriving (Show)

instance ToMarkup AppError where
    toMarkup (AppCFE cfe) = toMarkup $ commonFormErrorStr show cfe

type SimpleForm = Form (ServerPartT IO) [Input] AppError Html ()

type CFilterSig m = ([Maybe Power],[Maybe Cost],[Maybe Req]) -> Maybe [Color] -> Maybe [CSet] -> Maybe [Rarity] -> Maybe [CardType] -> m
type DFilterSig m = Maybe [Color] -> Maybe [CSet] -> Maybe [Rarity] -> Maybe [CardType] -> m

instance FormError AppError where
    type ErrorInputType AppError = [Input]
    commonFormError = AppCFE

data Filter = CardFilter
                { powMin :: Maybe Power
                , powMax :: Maybe Power
                , costMin :: Maybe Cost
                , costMax :: Maybe Cost
                , reqMin :: Maybe Req
                , reqMax :: Maybe Req
                , colors :: [Color]
                , sets :: [CSet]
                , types :: [CardType]
                , rarities :: [Rarity]
                }
             | DeckFilter
                { colors :: [Color]
                , sets :: [CSet]
                , types :: [CardType]
                , rarities :: [Rarity]
                }
        deriving (Eq, Ord, Read, Show)

mhfilter :: Filter -> IxSet GenCard -> IxSet GenCard
mhfilter c@CardFilter{..} = (if isJust powMin then (@>= (fromJust powMin)) else id) . (if isJust powMax then (@<= (fromJust powMax)) else id) . (if isJust costMin then (@>= (fromJust costMin)) else id) . (if isJust costMax then (@<= (fromJust costMax)) else id) . (if isJust reqMin then (@>= (fromJust reqMin)) else id) . (if isJust reqMax then (@<= (fromJust reqMax)) else id)

full :: [a] -> Bool
full [] = False
full _ = True

mcfilter :: Filter -> IxSet GenCard -> IxSet GenCard
mcfilter c@CardFilter{..} = (if full colors then (@+ colors) else id) . (if full sets then (@+ sets) else id) . (if full types then (@+ types) else id) . (if full rarities then (@+ rarities) else id)
mcfilter d@DeckFilter{..} = (if full colors then (@+ colors) else id) . (if full sets then (@+ sets) else id) . (if full types then (@+ types) else id) . (if full rarities then (@+ rarities) else id)

applyFilter :: Filter -> [GenCard]
applyFilter c@CardFilter{..} = toList $ mcfilter c . mhfilter c $ cardDB
applyFilter d@DeckFilter{..} = toList $ mcfilter d $ cardDB

renderFilter :: Filter -> Html
renderFilter fil = let gens = applyFilter fil in
    H.table $ do
      H.tr $ do
        mapM_ (H.td) headers
      H.tr $ do
        mapM_ cardLine gens
  where
    headers = ["#","Rarity","Type","Cost","Req.","Name","Power"]

cardLine :: GenCard -> Html
cardLine g@GenCard{..} =
  H.tr $ do
    H.td . toHtml . genset $ g
    H.td . toHtml . brief $ rar
    H.td . iconic $ ctype
    H.td . toHtml $ fromMaybe "" (show.val <$> mcost)
    H.td $ reqtify g
    H.td $ pronounce (unravel name, genset $ g)
    H.td $ empower g

iconic :: CardType -> Html
iconic x = let ipath = "res/icns/"++(show x)++".png" in
  H.img ! A.class_ "icon"
        ! A.src (toValue ipath)
      
cardForm :: ([Maybe Power],[Maybe Cost],[Maybe Req]) -> Maybe [Color] -> Maybe [CSet] -> Maybe [Rarity] -> Maybe [CardType] -> SimpleForm Filter
cardForm ([mnp,mxp],[mnc,mxc],[mnr,mxr]) mc ms mr mt =
  CardFilter
    <$> labelString "Power:"      ++> inputMin mnp
    <*> labelString "to"          ++> inputMax mxp            <++ br
    <*> labelString "Cost:"       ++> inputMin mnc
    <*> labelString "to"          ++> inputMax mxc            <++ br
    <*> labelString "Requirement:"++> inputMin mnr
    <*> labelString "to"          ++> inputMax mxr            <++ br
    <*> labelString "Color"       ++> sv show colorValues  mc <++ br
    <*> labelString "Set"         ++> sv show setValues    ms <++ br
    <*> labelString "Type"        ++> sv show typeValues   mt <++ br
    <*> labelString "Rarity"      ++> sv show rarityValues mr <++ br
    <*  inputSubmit "Filter"

sv f vs (Nothing) = selectMultiple (map (\x -> (x, f x)) vs) (const False)
sv f vs (Just xs) = selectMultiple (map (\x -> (x, f x)) vs) (`elem`xs)
      
deckForm :: Maybe [Color] -> Maybe [CSet] -> Maybe [Rarity] -> Maybe [CardType] -> SimpleForm Filter
deckForm mc ms mr mt =
  DeckFilter
    <$> labelString "Color"       ++> sv show colorValues  mc <++ br
    <*> labelString "Set"         ++> sv show setValues    ms <++ br
    <*> labelString "Type"        ++> sv show typeValues   mt <++ br
    <*> labelString "Rarity"      ++> sv show rarityValues mr <++ br
    <*  inputSubmit "Filter"

labelString :: String -> SimpleForm ()
labelString = label
