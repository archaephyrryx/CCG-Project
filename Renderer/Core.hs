{-# LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-} 
module Renderer.Core (
    module Renderer.Core.Internal
  , module Renderer.Core
  ) where

import Control.Applicative
import qualified Data.Text          as Strict
import qualified Data.Text.Lazy     as Lazy
import Data.Text.Lazy               ( Text )
import Text.Blaze ((!), ToValue(..), toValue)
import Text.Blaze.Html (toHtml)
import Happstack.Server
import qualified Text.Blaze.Html5 as H (hr)
---------------------------------------------------
import Util
import Language.Haskell.TH
import Renderer.Core.Internal
import Prelude hiding (div, span)

infixl 8 #+
infixl 8 #.
infixl 8 #
infixr 7 #:
infixl 7 #$

--- Operators
 
set :: ToValue v => String -> v -> Builder -> Builder
set a v = (>$((at a $ toValue v):))

(#) :: a -> (a -> b) -> b
(#) = flip ($)

(#.) :: Builder -> String -> Builder
(#.) b s = b # set "class" s

(#+) :: Builder -> [Rendered] -> Rendered
(#+) (renr,attrs) rens = renr attrs $ morph rens

(#$) :: Builder -> Rendered' -> Rendered
(#$) (renr,attrs) ren' = renr attrs ren'

(#:) :: Builder -> Rendered -> Rendered
(#:) (renr,attrs) rens = renr attrs $ orph rens

orph :: Rendered -> Rendered'
orph x = x

morph :: [Rendered] -> Rendered'
morph xs = sequence_ xs

collect :: [Rendered'] -> Rendered'
collect xs = sequence_ xs

--- Elements

hr :: Rendered
hr = H.hr

string :: String -> Rendered'
string str = toHtml str

$(makeElement "a")
$(makeElement "p")
$(makeElement "span")
$(makeElement "div")
$(makeElement "img")
$(makeElements ((:) <$> "h" <*> (show <$> [1..6])))
$(makeElements ["table", "tr", "th", "td"])
$(makeElements ["dl", "dd", "dt"])
$(makeElements ["ul", "li"])
$(makeElements ["b", "i"])

--- Attributes

$(makeAttr "src")
$(makeAttr "href")
$(makeAttr "text")
