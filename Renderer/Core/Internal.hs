{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Renderer.Core.Internal where

import Control.Applicative
import Data.Maybe

import Graphics.UI.Threepenny.Core (UI, Element, WriteAttr, attr, mkElement, set)
import qualified Graphics.UI.Threepenny.Core as UI ((#+), string)
import qualified Graphics.UI.Threepenny.Elements as UI (hr)
import Language.Haskell.TH
-----
import Util

type Rendered = UI Element
type Rendered' = [UI Element]
type Mattrs = [(WriteAttr Element String, String)]
type Builder = ((Mattrs -> Rendered' -> Rendered), Mattrs)
type Vacuum = ((Mattrs -> Rendered), Mattrs)

type Renderer a = a -> Rendered
type Renderer' a = a -> Rendered'

-- Library-specific operator code
orph :: Rendered -> Rendered'
orph = one

morph :: [Rendered] -> Rendered'
morph = id

collect :: [Rendered'] -> Rendered'
collect = concat

-- Library-specific non-builder elements
string :: String -> Rendered'
string = once UI.string

hr :: Rendered
hr = UI.hr

-- Library-specific attribute-maker
at :: String -> WriteAttr Element String
at x = attr x

atval :: String -> String -> Mattrs -> Mattrs
atval a v = ((at a, v):)

build :: Rendered -> Mattrs -> Rendered' -> Rendered
build b as = (UI.#+) (foldl (flip $ uncurry set) b as)

vacate :: Rendered -> Mattrs -> Rendered
vacate = foldl (flip $ uncurry set)

-- TH library->Renderer declarations

makeElement :: String -> Q [Dec]
makeElement s = do id <- newName s
                   at <- newName "at"
                   ch <- newName "ch"
                   return $ [ ValD (VarP id) (NormalB (SigE (TupE
                           [ LamE [VarP at, VarP ch] (AppE (AppE (AppE (VarE 'build)
                                 (AppE (VarE 'mkElement) (LitE (StringL s))))
                               (VarE at)) (VarE ch))
                           , (ListE [])]) (ConT ''Builder))) [] ]

makeElements :: [String] -> Q [Dec]
makeElements ss = fmap concat $ sequence (map makeElement ss)

makeVacuum :: String -> Q [Dec]
makeVacuum s = do id <- newName s
                  at <- newName "at"
                  return $ [ ValD (VarP id) (NormalB (SigE (TupE
                           [ LamE [VarP at] (AppE (AppE (VarE 'vacate)
                                 (AppE (VarE 'mkElement) (LitE (StringL s))))
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
