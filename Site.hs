{-# LANGUAGE FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, ScopedTypeVariables,
    TypeFamilies, TypeSynonymInstances,
    OverlappingInstances,
    QuasiQuotes, OverloadedStrings #-}

module Site where

import Control.Applicative        ((<$>))
import Control.Monad
import Control.Monad.Identity     (Identity(runIdentity))
import Data.List (intercalate)
import Data.String                (IsString(fromString))
import Data.Text.Lazy             (Text)
import qualified Data.Text        as Strict
import qualified Data.Text.Lazy   as Lazy
import Data.Char                  (toLower)
import Happstack.Server
import Happstack.Server.XMLGenT
import HSP
import HSP.Monad                  (HSPT(..))
import Language.Haskell.HSX.QQ    (hsx)
import Application
import Text.Reform.Happstack
import Text.Reform
    ( CommonFormError(..), Form, FormError(..), Proof(..), (++>)
    , (<++), commonFormErrorStr, decimal, prove
    , transformEither, transform )
import Text.Reform.Happstack
import Text.Reform.HSP.Text
import qualified Pages.Vanilla as Vanilla
import qualified Pages.Reform as Reformed
import Pages.Reform (cardHtml, deckHtml, CFilterSig, DFilterSig)
import Pages.Common
import Pages.Card
import CCG.Cards.Common

sitename = "HappleJack"

homePage :: Page
homePage = page home

page :: Html -> Page
page = (toResponse<$>).unHSPT.unXMLGenT

home :: Html
home = base pagename $
  [hsx|
    <div>
      <h1><% pagename %></h1>
      <p>Welcome to <% pagename %>, an in-development website for the MLP:CCG.</p>
      <p>This site was inspired by, and modelled on, <a href="http://ponyhead.com">PonyHead</a></p>
      <p>This site has several components that behave similarly to those of
      PonyHead, as well as several novel features. Bear in mind, though, that this
      site is still under development, so those features might be missing, or buggy
      if they are present.</p>
    </div> :: Html
  |]
  where
    pagename = sitename

card :: CFilterSig
card x mc ms mr mt = base pagename (cardHtml x mc ms mr mt)
  where
    pagename = sitename ++ ": Cards"

deck :: DFilterSig
deck mc ms mr mt = base pagename (deckHtml mc ms mr mt)
  where
    pagename = sitename ++ ": Deck Builder"
