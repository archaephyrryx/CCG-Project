{-# Language TemplateHaskell, QuasiQuotes #-}
module Data.Dimorph.Quasi where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Dimorph.Language
import Data.Dimorph.Parse
import Data.Dimorph.Alt
import Data.Dimorph.Prim
import Util.Conditional (if_)
import Data.Either

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
quoteDiExp s =  do
  let MDef (Iso (TName t1) (TName t2)) xs = fromRight $ dimorphParse s
  xss <- lift xs
  xn <- newName "x"
  yn <- newName "y"
  mapsto <- [| (:<=>:) |]
  let a = mkName t1
      b = mkName t2
  m <- newName "mappings"
  return (LetE [ValD (VarP m)
                     (NormalB
                       ((AppE
                         (AppE (VarE 'map)
                         (LamE
                           [ (ConP 'QMap [ ConP 'LHS [ConP 'Unary [ConP 'CName [VarP xn]]]
                                         , ConP 'RHS [ConP 'Unary [ConP 'CName [VarP yn]]]
                                         ]
                             )
                           ]
                           (InfixE
                             (Just (SigE (AppE (VarE 'read) (VarE xn)) (ConT a)))
                             mapsto
                             (Just (SigE (AppE (VarE 'read) (VarE yn)) (ConT b)))
                           )
                      )) xss)))
                      [] ]
           (SigE
             (AppE
               (AppE
                 (AppE (VarE 'if_)
                       (AppE
                         (VarE 'consistent)
                         (VarE m)
                       )
                 )
                 (AppE
                   (AppE (ConE 'Dimorph)
                         (AppE (AppE (VarE 'map) (VarE 'lhs)) (VarE m)))
                         (AppE (AppE (VarE 'map) (VarE 'rhs)) (VarE m)))
                 )
                 ((AppE (VarE 'error) (LitE (StringL "inconsistency in dimorph definition")))))
                 (AppT (AppT (ConT ''Dimorph) (ConT a)) (ConT b))))

fromRight :: Either a b -> b
fromRight x = case x of
                (Left _) -> error "fromRight"
                (Right x) -> x

quoteDiDec :: String -> Q [Dec]
quoteDiDec s =  do
  let MDef (Iso (TName t1) (TName t2)) xs = fromRight $ dimorphParse s
  xss <- lift xs
  xn <- newName "x"
  yn <- newName "y"
  mapsto <- [| (:<=>:) |]
  dim <- [| Dimorph |]
  dimt <- [t| Dimorph |]
  let a = mkName t1
      b = mkName t2
  m <- newName "mappings"
  return [ ValD (VarP (mkName ("di" ++ '\'':t1 ++ '\'':t2)))
           (NormalB
           (SigE
             (AppE
               (AppE
                 (AppE (VarE 'if_)
                       (AppE
                         (VarE 'consistent)
                         (VarE m)
                       )
                 )
                 (AppE
                   (AppE dim
                         (AppE (AppE (VarE 'map) (VarE 'lhs)) (VarE m)))
                         (AppE (AppE (VarE 'map) (VarE 'rhs)) (VarE m)))
                 )
                 ((AppE (VarE 'error) (LitE (StringL "inconsistency in dimorph definition")))))
                 (AppT (AppT dimt (ConT a)) (ConT b))))
               [ValD (VarP m)
                     (NormalB
                     (AppE (AppE (VarE 'map)
                       (LamE
                           [ (ConP 'QMap [ ConP 'LHS [ConP 'Unary [ConP 'CName [VarP xn]]]
                                         , ConP 'RHS [ConP 'Unary [ConP 'CName [VarP yn]]]
                                         ]
                             )
                           ]
                           (InfixE
                             (Just (SigE (AppE (VarE 'read) (VarE xn)) (ConT a)))
                             mapsto
                             (Just (SigE (AppE (VarE 'read) (VarE yn)) (ConT b)))
                           )
                      )) xss))
                      [] ] ]

bijectDec :: String -> Q [Dec]
bijectDec s =  do
  let MDef (Iso (TName t1) (TName t2)) xs = fromRight $ dimorphParse s
  xss <- lift xs
  xn <- newName "x"
  yn <- newName "y"
  mapsto <- [| (:<=>:) |]
  let a = mkName t1
      b = mkName t2
  return [ InstanceD [] (AppT (AppT (ConT ''Bijection) (ConT a)) (ConT b))
      [ValD (VarP (mkName "mappings"))
            (NormalB
              (AppE
                (AppE (VarE 'map)
                  (LamE
                    [ (ConP 'QMap [ ConP 'LHS [ConP 'Unary [ConP 'CName [VarP xn]]]
                                  , ConP 'RHS [ConP 'Unary [ConP 'CName [VarP yn]]]
                                  ]) ]
                    (InfixE
                      (Just (SigE (AppE (VarE 'read) (VarE xn)) (ConT a)))
                      mapsto
                      (Just (SigE (AppE (VarE 'read) (VarE yn)) (ConT b)))
                    ))) xss)) [] ] ]
