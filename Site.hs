{-# LANGUAGE FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, ScopedTypeVariables,
    TypeFamilies, TypeSynonymInstances,
    OverlappingInstances,
    QuasiQuotes, OverloadedStrings #-}

module Site ( homePage, home
            , cardPage, card
            , deckPage, deck
            , page, paginate
            ) where

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
import Forms
import qualified Pages.Vanilla as Vanilla
import qualified Pages.Reform as Reformed
import Pages.Reform (cardHtml, deckHtml)
import Pages.Common

sitename = "HappleJack"

homePage :: Pager
homePage = page.paginate $ home

cardPage :: Pager
cardPage = page.paginate $ card

deckPage :: Pager
deckPage = page.paginate $ deck

page :: Paginator -> Pager
page = (toResponse<$>)

paginate :: Html -> Paginator
paginate = unHSPT . unXMLGenT

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

card :: Html
card = base pagename cardHtml
  where
    pagename = sitename ++ ": Cards"

deck :: Html
deck = base pagename deckHtml
  where
    pagename = sitename ++ ": Deck Builder"
