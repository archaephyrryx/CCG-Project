module Data.Dimorph.Parse where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative hiding (many)

import Data.Dimorph.Language

def = emptyDef { identStart = letter
               , identLetter = alphaNum
               , opStart = oneOf "<"
               , opLetter = oneOf "<=>"
               , reservedOpNames = ["<=>"]
               , reservedNames = ["iso"]
               }

TokenParser { parens = m_parens
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , semiSep1 = m_semiSep1
            , whiteSpace = m_whiteSpace } = makeTokenParser def

defParse :: Parser MDef
defParse = do m_whiteSpace
              iso <- isoParse
              maps <- many mappingParse
              eof
              return (MDef iso maps)

isoParse :: Parser Iso
isoParse = do m_reserved "iso"
              m_whiteSpace
              a <- (do
                       x <- typParse
                       return x)
              m_whiteSpace
              b <- (do
                       x <- typParse
                       return x)
              return (Iso a b)


typParse :: Parser TName
typParse = TName <$> m_identifier

mappingParse :: Parser QMapping
mappingParse = do
                  l <- tagParse
                  m_reservedOp "<=>"
                  r <- tagParse
                  return (QMap (LHS (Unary l)) (RHS (Unary r)))


tagParse :: Parser CName
tagParse = CName <$> m_identifier

dimorphParse :: String -> Either ParseError MDef
dimorphParse = runP defParse () ""
