module Data.Invotomorph.Parse where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language import Control.Applicative

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

defParse :: Parsec String () RDef
defParse = do m_whiteSpace
              auto <- autoParse
              rules <- many rulingParse
              eof
              return (RDef auto rules)

isoParse :: Parsec String () Iso
isoParse = do m_reserved "auto"
              m_whiteSpace
              a <- (do
                       x <- typParse
                       return x)
              return (Auto a)


typParse :: Parsec String () TName
typParse = m_identifier

mappingParse :: Parsec String () QRule
mappingParse = do
                  l <- tagParse
                  m_reservedOp "<->"
                  r <- tagParse
                  return (QRule (LHS (Unary l)) (RHS (Unary r)))


tagParse :: Parsec String () CName
tagParse = m_identifier
