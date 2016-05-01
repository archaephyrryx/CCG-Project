{-# Language TemplateHaskell, QuasiQuotes #-}
module Data.Dimorph.Quasi where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.TypeCast
import Data.Dimorph.Language
import Data.Dimorph.Parse
import Data.Dimorph.Alt
import Data.Dimorph.Prim
import Util

diExp :: MDef -> Q Exp
diExp m@(MDef (Iso (TName t1) (TName t2)) xs) = do
  mexp <- qMDef m
  return (SigE
         (AppE (AppE (AppE (VarE 'if_)
               (AppE (VarE 'consistent) mexp))
               (AppE (AppE (ConE 'Dimorph)
                  (AppE (AppE (VarE 'map) (VarE 'lhs)) mexp))
                  (AppE (AppE (VarE 'map) (VarE 'rhs)) mexp)
               ))
                ((AppE (VarE 'error) (LitE (StringL "inconsistency in dimorph definition")))))
                (AppT (AppT (ConT ''Dimorph) (ConT t1)) (ConT t2)))

qMDef :: MDef -> Q Exp
qMDef (MDef _ []) = return $ ListE []
qMDef (MDef i (x:xt)) =
  do
    this <- qMapping i x
    rest <- qMDef (MDef i xt)
    return $ (InfixE (Just this) (ConE '(:)) (Just rest))

qMapping :: Iso -> QMapping -> Q Exp
qMapping (Iso (TName t1) (TName t2)) x =
  do
    let (c1,c2) = sides x
    l <- sig c1 t1
    r <- sig c2 t2
    return $ (InfixE (Just l) (ConE '(:<=>:)) (Just r))


sides :: QMapping -> (Name,Name)
sides (QMap (LHS (Unary (CName x))) (RHS (Unary (CName y)))) = (x,y)
---



quoteDiPat :: String -> Q Pat
quoteDiPat = undefined

quoteDiType :: String -> Q Type
quoteDiType = undefined

dimorph :: QuasiQuoter
dimorph = QuasiQuoter
        { quoteExp = quoteDiExp
        , quoteDec = quoteDiDec
        , quotePat = quoteDiPat
        , quoteType = quoteDiType
        }

biject :: QuasiQuoter
biject = QuasiQuoter
       { quoteExp = undefined
       , quoteDec = bijectDec
       , quotePat = undefined
       , quoteType = undefined
       }

quoteDiExp :: String -> Q Exp
quoteDiExp = diExp . fromRight . dimorphParse

quoteDiDec :: String -> Q [Dec]
quoteDiDec s = do
  let m@(MDef (Iso (TName t1) (TName t2)) _) = fromRight $ dimorphParse s
      nam = mkName ("di" ++ '\'':(showName t1) ++ '\'':(showName t2))
  e <- diExp m
  return [ ValD (VarP nam) (NormalB e) [] ]

bijectDec :: String -> Q [Dec]
bijectDec s =  do
  let m@(MDef (Iso (TName t1) (TName t2)) _) = fromRight $ dimorphParse s
  e <- qMDef m
  return [ InstanceD [] (AppT (AppT (ConT ''Bijection) (ConT t1)) (ConT t2))
      [ValD (VarP (mkName "mappings")) (NormalB e) [] ] ]
