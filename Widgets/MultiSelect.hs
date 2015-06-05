{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances, ViewPatterns, QuasiQuotes, OverloadedStrings #-}

module Widgets.MultiSelect where

import HSP
import qualified Text.Reform.Generalized as G
import Text.Reform.Backend
import Text.Reform
import Data.Text.Lazy             (Text)
import Language.Haskell.HSX.QQ    (hsx)
import Data.Maybe
import CCG
import Control.Applicative

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
import Pages.Common

selectFilter :: Html
selectFilter =
  [hsx|
    <div id="selectFilter">
      <td>
        <label for="setFilter">Set</label>
        <select name="setFilter" multiple="true">
          <option value="0">Premiere</option>
          <option value="1">Canterlot Nights</option>
          <option value="2">Rock and Rave</option>
          <option value="3">Celestial Solstice</option>
          <option value="4">Crystal Games</option>
        </select>
      </td>
      <td>
        <label for="colorFilter">Color</label>
        <select name="colorFilter" multiple="true">
          <option value="0">None</option>
          <option value="1">Blue</option>
          <option value="2">Orange</option>
          <option value="3">Pink</option>
          <option value="4">Purple</option>
          <option value="5">White</option>
          <option value="6">Yellow</option>
        </select>
      </td>
      <td>
        <label for="typeFilter">Type</label>
        <select name="typeFilter" multiple="true">
          <option value="0">Mane</option>
          <option value="1">Friend</option>
          <option value="2">Event</option>
          <option value="3">Resource</option>
          <option value="4">Troublemaker</option>
          <option value="5">Problem</option>
        </select>
      </td>
      <td>
        <label for="rarityFilter">Rarity</label>
        <select name="rarityFilter" multiple="true">
          <option value="0">Fixed</option>
          <option value="1">Common</option>
          <option value="2">Uncommon</option>
          <option value="3">Rare</option>
          <option value="4">Ultra-Rare</option>
          <option value="5">Promo</option>
        </select>
      </td>
    </div> :: Html
  |]
