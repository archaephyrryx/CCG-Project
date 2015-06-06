{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Renderer.Core.Internal where

import Application
import qualified Data.Text as Strict
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Lazy
-----
import Control.Applicative
import Data.Maybe
import Data.Text.Lazy (Text, pack)
import Happstack.Server
import Happstack.Server.HSP.HTML
import Happstack.Server.Internal.Monads (ServerPartT)
import Happstack.Server.XMLGenT
import HSP
import HSP.Monad (HSPT(..))
import HSP.XMLGenerator
import HSP.XML (XML(..))
import Language.Haskell.HSX.QQ      ( hsx )
import Language.Haskell.TH

type Rendered = Html
type Rendered' = GCL
type Mattrs = [GAL]

type Builder = ((Mattrs -> Rendered' -> Rendered), Mattrs)
type Vacuum = ((Mattrs -> Rendered), Mattrs)

type Renderer a = a -> Rendered
type Renderer' a = a -> Rendered'

--- Instance declarations

instance (Functor m, Monad m) =>
         EmbedAsAttr (App' m) (Attr Text Strict.Text) where
    asAttr (n := v) = asAttr (n := Lazy.fromStrict v)

instance (Functor m, Monad m) =>
         EmbedAsAttr (App' m) (Attr Text String) where
    asAttr (n := v) = asAttr (n := Lazy.pack v)

--

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

atval :: String -> String -> Mattrs -> Mattrs
atval t s = (asAttr ((pack t) := s):)

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
                                (TupE [ConE 'Nothing, AppE (VarE 'fromStringLit) (LitE (StringL s))]))
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
