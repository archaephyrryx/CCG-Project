module Data.TypeCast where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Util

sig :: Name -> Name -> Q Exp
sig e t = let e' = showName e
              t' = showName t
           in case t' of
                "String" -> return $ LitE (StringL (restring e'))
                "Char" -> return $ LitE (CharL (rechar e'))
                "Bool" -> return $ LitE (BoolE (rebool e'))
                "Integer" -> return $ LitE (IntegerL (reinteger e'))
                "Int" -> return $ LitE (IntegerL (reinteger e'))
                _ -> return $ (SigE (ConE e) (ConT t))

restring :: String -> String
restring = full?.

rechar :: String -> Char
rechar

rebool :: String -> Bool
rebool

reinteger :: String -> Integer
reinteger

