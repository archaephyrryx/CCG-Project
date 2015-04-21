{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Renderer.Core.Internal where

import Control.Applicative
import Data.Maybe
import Happstack.Server
import Happstack.Server.Internal.Monads (ServerPartT)
import Text.Blaze.Html5 (toValue, toHtml)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal (StaticString(..), attribute, MarkupM(..), Tag(..), stringTag)
import Text.Blaze.Internal
import Text.Blaze.Html
import Text.Blaze (ToValue)
import Language.Haskell.TH
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-----
import GHC.Tuple

type Rendered = Html
type Rendered' = Rendered
type Mattrs = [Attribute]
type Builder = ((Mattrs -> Rendered' -> Rendered), Mattrs)

type Renderer a = a -> Rendered
type Renderer' a = a -> Rendered'

stat :: String -> StaticString
stat s = let t = T.pack s
         in StaticString (s ++) (T.encodeUtf8 t) t

at :: String -> (AttributeValue -> Attribute)
at x = attribute (stringTag x) (stringTag $ (' ':x++"=\""))

build :: (Rendered' -> Rendered) -> Mattrs -> Rendered' -> Rendered
build = foldl (!)

elmer :: String -> (Rendered' -> Rendered)
elmer s = Parent (stat s) (stat ("<"++s)) (stat ("</"++s++">"))

makeElement :: String -> Q [Dec]
makeElement s = do id <- newName s
                   at <- newName "at"
                   ch <- newName "ch"
                   return $ [ ValD (VarP id) (NormalB (SigE (TupE
                           [ LamE [VarP at, VarP ch] (AppE (AppE (AppE (VarE 'build)
                                 (AppE (VarE 'elmer) (LitE (StringL s))))
                               (VarE at)) (VarE ch))
                           , (ListE [])]) (ConT ''Builder))) [] ]

makeElements :: [String] -> Q [Dec]
makeElements ss = fmap concat $ sequence (map makeElement ss)

makeAttr :: String -> Q [Dec]
makeAttr s = do id <- newName s
                return [ ValD (VarP id) (NormalB (LitE (StringL s))) [] ]

makeAttrs :: [String] -> Q [Dec]
makeAttrs ss = fmap concat $ sequence (map makeAttr ss)
