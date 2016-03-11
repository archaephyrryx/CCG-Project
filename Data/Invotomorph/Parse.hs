module Data.Invotomorph.Parse where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative hiding (many)

import Data.Invotomorph.Language

def = emptyDef { identStart = letter
               , identLetter = alphaNum
               , opStart = oneOf "<"
               , opLetter = oneOf "<->"
               , reservedOpNames = ["<->"]
               , reservedNames = ["auto"]
               }

TokenParser { parens = m_parens
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , semiSep1 = m_semiSep1
            , whiteSpace = m_whiteSpace } = makeTokenParser def

defParse :: Parser RDef
defParse = do m_whiteSpace
              auto <- autoParse
              rules <- many rulingParse
              eof
              return (RDef auto rules)

autoParse :: Parser Auto
autoParse = do m_reserved "auto"
               m_whiteSpace
               a <- (do x <- typParse
                        return x)
               return (Auto a)


rulingParse :: Parser QRule
rulingParse = do
                  l <- tagParse
                  m_reservedOp "<->"
                  r <- tagParse
                  return (QRule (LHS (Unary l)) (RHS (Unary r)))


tagParse :: Parser CName
tagParse = CName <$> m_identifier

typParse :: Parser TName
typParse = TName <$> m_identifier

invotoParse :: String -> Either ParseError RDef
invotoParse = runP defParse () ""
