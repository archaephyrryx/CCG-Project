{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Renderer.Core.Internal where

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
import Data.Text.Lazy (Text)
-----
import GHC.Tuple

type App' m = HSPT XML (ServerPartT m)
type App m  = XMLGenT (App' m)
type Html = App IO XML
type Html' = App' IO XML
type GCL = GenChildList (App' IO)
type GAL = GenAttributeList (App' IO)

type Rendered = Html
type Rendered' = GCL
type Mattrs = [GAL]
type Builder = ((Mattrs -> Rendered' -> Rendered), Mattrs)

type Renderer a = a -> Rendered
type Renderer' a = a -> Rendered'

appt :: Type -> Type
appt f = AppT (ConT ''XMLGenT) (AppT (AppT (ConT ''HSPT) (ConT ''XML)) (AppT (ConT ''ServerPartT) f))

gcl :: Type -> Type
gcl f = AppT (ConT ''GenChildList) (AppT (AppT (ConT ''HSPT) (ConT ''XML)) (AppT (ConT ''ServerPartT) f))

gal :: Type -> Type
gal f = AppT (ConT ''GenAttributeList) (AppT (AppT (ConT ''HSPT) (ConT ''XML)) (AppT (ConT ''ServerPartT) f))

tupt :: Type -> Type -> Type
tupt x y = AppT (AppT (TupleT 2) x) y

funt :: Type -> Type -> Type
funt x y = AppT (AppT ArrowT x) y

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


makeAttr :: String -> Q [Dec]
makeAttr s = do id <- newName s
                return [ ValD (VarP id) (NormalB (SigE (LitE (StringL s)) (ConT ''Text))) [] ]

makeAttrs :: [String] -> Q [Dec]
makeAttrs ss = fmap concat $ sequence (map makeAttr ss)
