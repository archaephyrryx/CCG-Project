{-# LANGUAGE DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, OverlappingInstances,
    IncoherentInstances, OverloadedStrings,
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
import Data.String                  ( IsString(fromString) ) 
import Data.Text.Lazy               ( Text )
import Happstack.Server
--import Happstack.Server.XMLGenT
import qualified Data.Text          as Strict
import qualified Data.Text.Lazy     as Lazy
{-import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze ((!)) -}
import Text.Blaze.Html5 (Html, ToMarkup, toHtml, toMarkup, toValue)
import Text.Blaze.Internal (MarkupM)
import Text.Reform
import Text.Reform.Happstack
import Text.Reform.Blaze.String
--------------------------------------------------
import API.Filter
import CCG hiding (Text)
import Util
--------------------------------------------------
import Application
import Widgets.Minmax
import Renderer.Core hiding (form)
import App.Core.Base (template)
---------------------------------------------------

newtype AppError = AppCFE (CommonFormError [Input])
    deriving (Show)
{-
instance ToMarkup AppError where
  toMarkup (AppCFE cfe) = toMarkup $ commonFormErrorStr show cfe
-}
type SimpleForm = Form (ServerPartT IO) [Input] AppError Rendered ()

instance FormError AppError where
    type ErrorInputType AppError = [Input]
    commonFormError = AppCFE

cardForm :: Filter -> SimpleForm Filter
cardForm f =
  CardFilter
    <$> labelText "Power:"      ++> inputMin . powMin $ f
    <*> labelText "to"          ++> inputMax . powMax $ f   <++ br
    <*> labelText "Cost:"       ++> inputMin . costMin $ f
    <*> labelText "to"          ++> inputMax . costMax $ f  <++ br
    <*> labelText "Requirement:"++> inputMin . reqMin $ f
    <*> labelText "to"          ++> inputMax . reqMax $ f   <++ br
    <*> labelText "Color"       ++> svs colorValues . colors $ f <++ br
    <*> labelText "Set"         ++> svs setValues   . sets   $ f <++ br
    <*> labelText "Type"        ++> svs typeValues  . types  $ f <++ br
    <*> labelText "Rarity"      ++> svs rarityValues. rarities $ f <++ br
    <*  inputSubmit "Filter"

svs = sv show
sv f vs [] = selectMultiple (map (\x -> (x, f x)) vs) (const False)
sv f vs xs = selectMultiple (map (\x -> (x, f x)) vs) (`elem`xs)

deckForm :: Filter -> SimpleForm Filter
deckForm f =
  DeckFilter
    <$> labelText "Color"       ++> svs colorValues . colors $ f <++ br
    <*> labelText "Set"         ++> svs setValues   . sets   $ f <++ br
    <*> labelText "Type"        ++> svs typeValues  . types  $ f <++ br
    <*> labelText "Rarity"      ++> svs rarityValues. rarities $ f <++ br
    <*  inputSubmit "Filter"

cardHtml :: Renderer Filter
cardHtml f = do
    let action = "/card" :: Text
    result <- happstackEitherForm (form action) "card" (cardForm f)
    case result of
        (Left formHtml) ->
            template "Card Form" formHtml
        (Right flt) ->
            template "Card Result" $ [renderFilter flt]

deckHtml :: Renderer Filter
deckHtml f = do
    let action = "/deck" :: Text
    result <- happstackEitherForm (form action) "deck" (deckForm f)
    case result of
        (Left formHtml) ->
            template "Deck Form" formHtml
        (Right flt) ->
            template "Deck Result" $ [renderFilter flt]

renderFilter :: Renderer Filter
renderFilter = 
