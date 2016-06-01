module Data.Dimorph.Parse where

import Language.Haskell.TH
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative hiding (many, (<|>))

import Data.Dimorph.Language

def = emptyDef { identStart = letter <|> char '_'
               , identLetter = alphaNum <|> char '_'
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
            , whiteSpace = m_whiteSpace
            , lexeme = m_lexeme } = makeTokenParser def

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


typParse :: Parser Isotype
typParse =
        try tuParse
    <|> try tvParse
    <|> try tbParse
    <|> try ttParse
    <?> "unrecognized type expression"

tuParse, tvParse, tbParse, ttParse :: Parser Isotype
tuParse = Mono <$> ttagParse
tvParse = Poly <$> vParse
tbParse = do
  c <- ttagParse
  t <- vParse
  return $ Bin c t
ttParse = do
  c <- ttagParse
  t <- vParse
  s <- vParse
  return $ Tern c t s

mappingParse :: Parser QMapping
mappingParse = do
                  l <- m_lexeme termParse
                  m_lexeme $ m_reservedOp "<=>"
                  r <- m_lexeme termParse
                  return $ QMap (LHS l) (RHS r)

termParse :: Parser Term
termParse =
      try ternaryParse
  <|> try binaryParse
  <|> try unaryParse
  <|> try varParse
  <|> try constParse
  <?> "unrecognized term expression"

atomParse :: Parser Atom
atomParse =
      try (AVar <$> vParse)
  <|> try (ACon <$> tagParse)
  <?> "unrecognized atom"

intParse :: Parser Integer
intParse = do
  digits <- many1 digit
  let n = foldl (\x d -> 10*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

vParse :: Parser VName
vParse = VName . mkName <$> m_lexeme var_ident
  where
    var_ident :: Parser String
    var_ident = do
      x <- lower
      l <- many (alphaNum <|> oneOf "_'")
      return (x:l)

tagParse :: Parser CName
tagParse = CName . mkName <$> m_lexeme tag_ident

ttagParse :: Parser TName
ttagParse = TName . mkName <$> m_lexeme tag_ident

tag_ident :: Parser String
tag_ident = do
  x <- upper
  l <- many (alphaNum <|> oneOf "_'")
  m_whiteSpace
  return (x:l)


constParse, varParse, unaryParse, binaryParse, ternaryParse :: Parser Term
constParse = Constant <$> intParse
varParse = Var <$> vParse
unaryParse = Unary <$> tagParse
binaryParse = do
  c <- tagParse
  m_whiteSpace
  x <- atomParse
  return $ Binary c x
ternaryParse = do
  c <- tagParse
  m_whiteSpace
  x <- atomParse
  m_whiteSpace
  y <- atomParse
  return $ Ternary c x y

dimorphParse :: String -> Either ParseError MDef
dimorphParse = runP defParse () ""
