{-# LANGUAGE DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, OverlappingInstances,
    IncoherentInstances,
    OverloadedStrings, QuasiQuotes, RecordWildCards,
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
import Data.String                  ( IsString(fromString) ) 
import qualified Data.Text          as Strict
import qualified Data.Text.Lazy     as Lazy
import Data.Text.Lazy               ( Text )
import Happstack.Server
import Happstack.Server.HSP.HTML
import Happstack.Server.XMLGenT
import HSP
import HSP.Monad                    ( HSPT(..) )
import Language.Haskell.HSX.QQ      ( hsx )
import Text.Reform.Happstack
import Text.Reform.HSP.Text

import Text.Reform
    ( CommonFormError(..), Proof(..), (++>), (<++), commonFormErrorStr
    , Form, FormError(..), decimal, prove, transformEither, transform )
--------------------------------------------------
import API.Database
import API.Filter
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
import CCG hiding (Text)
import Util
--------------------------------------------------
import Application
import Reformation
import Renderer.Core
import Pages.Common (template)
import Pages.Card (reqtify, empower, pronounce)
---------------------------------------------------

newtype AppError = AppCFE (CommonFormError [Input])
    deriving (Show)

type SimpleForm = Form (AppT IO) [Input] AppError [Html] ()

instance (Functor m, Monad m) => EmbedAsChild (AppT' m) AppError where
  asChild (AppCFE cfe)      =
     asChild $ commonFormErrorStr show cfe

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
deckForm mc ms mr mt =
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
