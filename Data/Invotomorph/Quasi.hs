module Data.Invotomorph.Quasi where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Invotomorph.Language
import Data.Invotomorph.Parse

invoto :: QuasiQuoter
invoto = QuasiQuoter
        { quoteExp = quoteIvExp
        , quoteDec = quoteIvDec
        , quotePat = quoteIvPat
        , quoteType = quoteIvType
        }

quoteIvExp :: String -> Q Exp
quoteIvExp = lift
quoteIvDec :: String -> Q [Dec]
quoteIvDec = undefined
quoteIvPat :: String -> Q Pat
quoteIvPat = undefined
quoteIvType :: String -> Q Type
quoteIvType = undefined
