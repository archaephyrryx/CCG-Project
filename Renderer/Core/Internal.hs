{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Renderer.Core.Internal where

import Control.Applicative
import Data.Maybe
import Happstack.Server
import Happstack.Server.Internal.Monads (ServerPartT)
import Happstack.Server.HSP.HTML
import Happstack.Server.XMLGenT
import HSP
import HSP.Monad (HSPT(..))
import HSP.XML (XML(..))
import HSP.XMLGenerator
import Language.Haskell.TH
import Data.Text.Lazy (Text, pack)
-----
import GHC.Tuple

type App' m = HSPT XML (ServerPartT m)
type App m  = XMLGenT (App' m)
type Html = App IO XML
type GCL = GenChildList (App' IO)
type GAL = GenAttributeList (App' IO)

type Rendered = Html
type Rendered' = GCL
type Mattrs = [GAL]

type Builder = ((Mattrs -> Rendered' -> Rendered), Mattrs)
type Vacuum = ((Mattrs -> Rendered), Mattrs)

type Renderer a = a -> Rendered
type Renderer' a = a -> Rendered'

atval :: String -> Strig -> Mattrs -> Mattrs
atval t s = (toAttr ((pack t) := s):)

makeElement :: String -> Q [Dec]
makeElement s = do id <- newName s
                   at <- newName "at"
                   ch <- newName "ch"
                   return $ [ ValD (VarP id) (NormalB (SigE (TupE
                           [ LamE [VarP at, VarP ch] (AppE (AppE (AppE (VarE 'genElement)
                                 (TupE [ConE 'Nothing, AppE (VarE 'fromStringLit) (LitE (StringL s))]))
                               (ListE [AppE (VarE 'asAttr) (VarE at)])) (ListE [(VarE ch)]))
                           , (ListE [])]) (ConT ''Builder))) [] ]

makeElements :: [String] -> Q [Dec]
makeElements ss = fmap concat $ sequence (map makeElement ss)

makeVacuum :: String -> Q [Dec]
makeVacuum s = do id <- newName s
                  at <- newName "at"
                  return $ [ ValD (VarP id) (NormalB (SigE (TupE
                            [ LamE [VarP at] (AppE (AppE (VarE 'genEElement)
                                (TupE [ConE 'Nothing, AppE (VarE 'fromStringLit) (LitE (StringL s))])
                                (ListE [AppE (VarE 'asAttr) (VarE at)]))
                            , (ListE [])]) (ConT ''Vacuum))) [] ]

makeAttr :: String -> Q [Dec]
makeAttr s = do id <- newName s
                return [ ValD (VarP id) (NormalB (LitE (StringL s))) [] ]

makeAttr' :: (String,String) -> Q [Dec]
makeAttr' (s,s') = do id <- newName s
                      return [ ValD (VarP id) (NormalB (LitE (StringL s'))) []]

makeAttrs :: [String] -> Q [Dec]
makeAttrs ss = fmap concat $ sequence (map makeAttr ss)
