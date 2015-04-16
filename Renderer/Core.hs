{-# LANGUAGE RecordWildCards, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-} 
module Renderer.Core (
    module Renderer.Core.Internal
  , module Renderer.Core
  ) where

import Application
import Control.Applicative
import qualified Data.Text          as Strict
import qualified Data.Text.Lazy     as Lazy
import Data.Text.Lazy               ( Text )
import Happstack.Server
import Happstack.Server.HSP.HTML
import Happstack.Server.XMLGenT
import HSP hiding (set)
import HSP.Monad                    ( HSPT(..) )
import Language.Haskell.HSX.QQ      ( hsx )
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

--- Instance declarations

instance (Functor m, Monad m) =>
         EmbedAsAttr (AppT' m) (Attr Text Strict.Text) where
    asAttr (n := v) = asAttr (n := Lazy.fromStrict v)

instance (Functor m, Monad m) =>
         EmbedAsAttr (AppT' m) (Attr Text String) where
    asAttr (n := v) = asAttr (n := Lazy.pack v)

--- Operators
 
set :: Text -> String -> Builder -> Builder
set t s = (>$((asAttr $ (t := s)):))

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
orph x = [hsx|<%><% x %></%>|]

morph :: [Rendered] -> Rendered'
morph xs = [hsx|<%><% sequence xs %></%>|]

collect :: [Rendered'] -> Rendered'
collect xs = [hsx|<%><% sequence xs %></%>|]

--- Elements

string :: String -> Rendered'
string str = [hsx| <%> <% str %> </%> :: Rendered' |]

hr :: Rendered
hr = [hsx| <hr/> :: Rendered |]

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
