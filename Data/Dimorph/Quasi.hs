module Data.Dimorph.Quasi where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Dimorph.Language
import Data.Dimorph.Parse

dimorph :: QuasiQuoter
dimorph = QuasiQuoter
        { quoteExp = quoteDiExp
        , quoteDec = quoteDiDec
        , quotePat = quoteDiPat
        , quoteType = quoteDiType
        }

quoteDiExp :: String -> Q Exp
quoteDiExp = lift
quoteDiDec :: String -> Q [Dec]
quoteDiDec = undefined
quoteDiPat :: String -> Q Pat
quoteDiPat = undefined
quoteDiType :: String -> Q Type
quoteDiType = undefined
