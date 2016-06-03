module Data.Morphism.Dimorph.Parse where

import Language.Haskell.TH
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative hiding (many, (<|>))

import Data.Morphism.Dimorph.Language
import Data.Morphism.Language
import Data.Morphism.Parse

defParse :: Parser MDef
defParse = do m_whiteSpace
              iso <- isoParse
              maps <- many (mappingParse)
              eof
              return (MDef iso maps)

isoParse :: Parser Iso
isoParse = do m_reserved "iso"
              a <- (do
                       x <- typParse
                       return x)
              b <- (do
                       x <- typParse
                       return x)
              return (Iso a b)

mappingParse :: Parser QMapping
mappingParse = do
                  l <- m_lexeme termParse
                  m_lexeme $ m_reservedOp "<=>"
                  r <- m_lexeme termParse
                  return $ QMap (LHS l) (RHS r)

dimorphParse :: String -> Either ParseError MDef
dimorphParse = runP defParse () ""
