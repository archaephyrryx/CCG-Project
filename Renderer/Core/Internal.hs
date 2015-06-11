{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Renderer.Core.Internal where

import Control.Applicative
import Data.Maybe
import Happstack.Server
import Happstack.Server.Internal.Monads (ServerPartT)
import qualified Text.Blaze.Html5 as H (hr)
import Text.Blaze.Html5 (toValue, toHtml)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Internal
import Text.Blaze.Html
import Text.Blaze (ToValue)
import Language.Haskell.TH
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-----
import Util

type Rendered = Html
type Rendered' = Rendered
type Mattrs = [Attribute]
type Builder = ((Mattrs -> Rendered' -> Rendered), Mattrs)
type Vacuum = ((Mattrs -> Rendered), Mattrs)

type Renderer a = a -> Rendered
type Renderer' a = a -> Rendered'

-- Library-specific operator code
orph :: Rendered -> Rendered'
orph x = x

morph :: [Rendered] -> Rendered'
morph xs = sequence_ xs

collect :: [Rendered'] -> Rendered'
collect xs = sequence_ xs

hr :: Rendered
hr = H.hr

-- Library-specific non-builder elements
string :: String -> Rendered'
string str = toHtml str

stat :: String -> StaticString
stat s = let t = T.pack s
         in StaticString (s ++) (T.encodeUtf8 t) t

at :: String -> (AttributeValue -> Attribute)
at x = attribute (stringTag x) (stringTag $ (' ':x++"=\""))

atval :: ToValue v => String -> v -> Mattrs -> Mattrs
atval a v = ((at a $ toValue v):)

vacate :: Rendered -> Mattrs -> Rendered
vacate = foldl (!)

build :: (Rendered' -> Rendered) -> Mattrs -> Rendered' -> Rendered
build = foldl (!)

vacant :: String -> Rendered
vacant s = Leaf (stat s) (stat ("<"++s)) (stat (">"))

elmer :: String -> (Rendered' -> Rendered)
elmer s = Parent (stat s) (stat ("<"++s)) (stat ("</"++s++">"))
-- TH library->Renderer declarations

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

makeVacuum :: String -> Q [Dec]
makeVacuum s = do id <- newName s
                  at <- newName "at"
                  return $ [ ValD (VarP id) (NormalB (SigE (TupE
                           [ LamE [VarP at] (AppE (AppE (VarE 'vacate)
                                 (AppE (VarE 'vacant) (LitE (StringL s))))
                               (VarE at))
                           , (ListE [])]) (ConT ''Vacuum))) [] ]

makeAttr :: String -> Q [Dec]
makeAttr s = do id <- newName s
                return [ ValD (VarP id) (NormalB (LitE (StringL s))) [] ]

makeAttr' :: (String,String) -> Q [Dec]
makeAttr' (s,s') = do id <- newName s
                      return [ ValD (VarP id) (NormalB (LitE (StringL s'))) []]

makeAttrs :: [String] -> Q [Dec]
makeAttrs ss = fmap concat $ sequence (map makeAttr ss)
