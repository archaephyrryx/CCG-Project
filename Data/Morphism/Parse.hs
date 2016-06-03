module Data.Morphism.Parse where

import Language.Haskell.TH
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative hiding (many, (<|>))

import Data.Morphism.Language

def = emptyDef { identStart = letter <|> char '_'
               , identLetter = alphaNum <|> char '_'
               , opStart = oneOf "<"
               , opLetter = oneOf "<-=>"
               , reservedOpNames = ["<=>","<->"]
               , reservedNames = ["iso","auto"]
               }

TokenParser { parens = m_parens
            , identifier = m_identifier
            , reservedOp = m_reservedOp
            , reserved = m_reserved
            , semiSep1 = m_semiSep1
            , whiteSpace = m_whiteSpace
            , lexeme = m_lexeme } = makeTokenParser def

typParse :: Parser Isotype
typParse =
        try tuParse
    <|> try tvParse
    <|> try tbParse
    <|> try ttParse
    <?> "unrecognized type expression"

typParse' :: Parser (VName,VName)
typParse' = vParse`chain`vParse
{-
termParse' :: Parser (Term,Term)
termParse' =
      try (m_parens typParse`chain`m_parens typParse)
  <|> try (m_parens terParse`chain`         typParse)
  <|> try (typParse         `chain`m_parens typParse)
  <|> try (tvParse          `chain`         typParse)
  <|> try (ttParse' (tvParse`chain`tvParse) `chain`  typParse)
  <|> try (ttParse          `chain`         typParse)
  <|> try (tbParse          `chain`         typParse)
  <|> try (tuParse          `chain`         typParse)
-}

tuParse, tvParse, tbParse, ttParse :: Parser Isotype
tuParse = Mono <$> ttagParse
tvParse = Poly <$> vParse
tbParse = tbParse' vParse
ttParse = ttParse' typParse'

tbParse' :: Parser VName -> Parser Isotype
tbParse' p = ttagParse >>= \c -> p >>= \t -> return $ Bin c t

ttParse' :: Parser (VName,VName) -> Parser Isotype
ttParse' p = ttagParse >>= \c -> p >>= \(t,s) -> return $ Tern c t s

termParse :: Parser Term
termParse =
      try ternaryParse
  <|> try binaryParse
  <|> try unaryParse
  <|> try varParse
  <|> try constParse
  <?> "unrecognized term expression"

atomParse :: Parser Term
atomParse =
      try varParse
  <|> try constParse
  <?> "unrecognized atom"

diatomic :: Parser (Term,Term)
diatomic = atomParse`chain`atomParse

chain :: Parser a -> Parser b -> Parser (a,b)
chain p q = m_lexeme p >>= \x -> m_lexeme q >>= \y -> return (x,y)

termParse' :: Parser (Term,Term)
termParse' =
      try (m_parens termParse`chain`m_parens termParse)
  <|> try (m_parens termParse`chain`         termParse)
  <|> try (termParse         `chain`m_parens termParse)
  <|> try (constParse        `chain`         termParse)
  <|> try (varParse          `chain`         termParse)
  <|> try (ternaryParse' diatomic `chain`    termParse)
  <|> try (ternaryParse      `chain`         termParse)
  <|> try (binaryParse       `chain`         termParse)
  <|> try (unaryParse        `chain`         termParse)

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
binaryParse = binaryParse' termParse
ternaryParse = ternaryParse' termParse'

binaryParse' :: Parser Term -> Parser Term
binaryParse' p = do
  c <- tagParse
  x <- m_lexeme $ p
  return $ Binary c x

ternaryParse' :: Parser (Term,Term) -> Parser Term
ternaryParse' p = do
  c <- tagParse
  (x,y) <- m_lexeme $ p
  return $ Ternary c x y
