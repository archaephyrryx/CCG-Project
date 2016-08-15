-- | General purpose parser for the common syntactic rules of the Invotomorph and Dimorph
-- grammars
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
import Control.Monad (void)

import Data.Morphism.Language

-- | Parser for `Isotype`
typParse :: Parser Isotype
typParse =
        try tuParse
    <|> try tvParse
    <|> try tbParse
    <|> try ttParse
    <?> "unrecognized type expression"

-- | Specialized parser for simultaneous parsing of the two type-expressions corresponding to the
-- two arguments of a '* -> * -> *'-kinded type. Due to non-recursion, this parser attempts to parse
-- only `VName` expressions, but in the case of recursion, would parse two `Isotype` expressions in
-- parallel, respecting the hierarchical binding precedences for undelimited expressions.
--
-- Note that, as the arguments for such a doubly-abstract type are of type `VName` rather than
-- `Isotype` (or @Poly VName@), this parser is different from the Dimorph `Iso`-parser
typParse' :: Parser (VName,VName)
typParse' = vParse`chain`vParse

-- | A phantom parser corresponding to `typParse'` for a recursive variant of Isotype; as Isotypes
-- are non-recursive, this function does nothing, except act as a placeholder for the commented
-- implementation of the theoretical function.
typParse'' :: Parser (Isotype,Isotype)
typParse'' = error "Isotypes aren't recursive (yet)"
  {-
      try (parenth typParse`chain`parenth typParse)
  <|> try (parenth typParse`chain`         typParse)
  <|> try (typParse         `chain`parenth typParse)
  <|> try (tvParse          `chain`         typParse)
  <|> try (ttParse' (tvParse`chain`tvParse) `chain`  typParse)
  <|> try (ttParse          `chain`         typParse)
  <|> try (tbParse          `chain`         typParse)
  <|> try (tuParse          `chain`         typParse)
  -}

-- | Alternative parsers corresponding to the four constructors of `Isotype`.
tuParse, tvParse, tbParse, ttParse :: Parser Isotype
tuParse = Mono <$> ttagParse
tvParse = Poly <$> vParse
tbParse = tbParse' vParse
ttParse = ttParse' typParse'

tbParse' :: Parser VName -> Parser Isotype
tbParse' p = ttagParse >>= \c -> p >>= \t -> return $ Bin c t

ttParse' :: Parser (VName,VName) -> Parser Isotype
ttParse' p = ttagParse >>= \c -> p >>= \(t,s) -> return $ Tern c t s

-- | Parser for `Term` expressions
termParse :: Parser Term
termParse =
      try ternaryParse
  <|> try binaryParse
  <|> try unaryParse
  <|> try varParse
  <|> try strConstParse
  <|> try constParse
  <?> "unrecognized term expression"

-- | Parser for atomic terms, namely variables, constants, and strings; as these three term
-- expressions are standalone, they terminate any parsing for the current term expression and signal
-- that any subsequent identifiers belong to a different parse-rule.
atomParse :: Parser Term
atomParse =
      try varParse
  <|> try strConstParse
  <|> try constParse
  <?> "unrecognized atom"


-- | Parser that consumes and returns two atoms using `atomParse`
diatomic :: Parser (Term,Term)
diatomic = atomParse`chain`atomParse

-- | Promotes a parser to a whitespace-aware parser that skips all non-breaking whitespace before
-- and after the parser it promoted
whiteout :: Parser a -> Parser a
whiteout p = do { skipMany nbSpace; x <- p; nobreaks; return x; }

-- | Simple parser that consumes a tab or space
nbSpace :: Parser Char
nbSpace = tab <|> char ' '

-- | Parser that consumes all whitespace until the end of the line, without consuming newlines
nobreaks :: Parser ()
nobreaks = do
  manyTill space (lookAhead $ (try (void endOfLine) <|> notFollowedBy nbSpace))
  return ()

-- | Promotes a parser to parse a parenthetical expression
parenth :: Parser a -> Parser a
parenth p = between (whiteout.string $ "(") (whiteout.string $ ")") p

-- | Combinator on two parsers that sequences its whitespace-promoted arguments
chain :: Parser a -> Parser b -> Parser (a,b)
chain p q = whiteout p >>= \x -> whiteout q >>= \y -> return (x,y)

-- | Specialized parser for the simultaneous parsing of the two term arguments of a ternary
-- constructor
termParse' :: Parser (Term,Term)
termParse' =
      try (parenth termParse`chain`parenth termParse)
  <|> try (parenth termParse`chain`         termParse)
  <|> try (termParse        `chain`parenth termParse)
  <|> try (constParse       `chain`         termParse)
  <|> try (strConstParse    `chain`         termParse)
  <|> try (varParse         `chain`         termParse)
  <|> try (ternaryParse' diatomic `chain`    termParse)
  <|> try (ternaryParse      `chain`         termParse)
  <|> try (binaryParse       `chain`         termParse)
  <|> try (unaryParse        `chain`         termParse)

-- | Simple parser for positive arbitrary-precision integers
intParse :: Parser Integer
intParse = do
  digits <- many1 digit
  let n = foldl (\x d -> 10*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

-- | Specialized parser for explicitly quoted string expressions. While both Morphism grammars
-- automatically convert expressions that appear to be unary constructors into Strings if they
-- correspond to a String-typed LHS or RHS, any strings that cannot be parsed as unary constructors
-- (i.e. with spaces, beginning with non-identifier characters, containing non-identifier
-- characters) can only be parsed if they are explicitly quoted.
strParse :: Parser String
strParse = do
  char '"'
  chars <- many strChar
  char '"'
  return $ concat chars
  where
    strChar :: Parser String
    strChar =
      return <$> unescaped
             <|> escaped
      where
        unescaped :: Parser Char
        unescaped = noneOf "\\\"\0\n\r\v\t\b\f"
        escaped :: Parser String
        escaped = do
          s <- char '\\'
          c <- oneOf "\\\"\0\n\r\v\t\b\f"
          return [s,c]

-- | Parser for `VName`, following the standard conventions for what constitutes a variable
-- identifier in Haskell
vParse :: Parser VName
vParse = VName . mkName <$> whiteout var_ident
  where
    var_ident :: Parser String
    var_ident = do
      x <- lower
      l <- many (alphaNum <|> oneOf "_'")
      return (x:l)

-- | Parser for "tags" (`CName`)
tagParse :: Parser CName
tagParse = CName . mkName <$> whiteout tag_ident

-- | Parser for "type-tags" (`TName`)
ttagParse :: Parser TName
ttagParse = TName . mkName <$> whiteout tag_ident

-- | Parser for non-variable identifiers (namely, type or constructor names)
tag_ident :: Parser String
tag_ident = do
  x <- upper
  l <- many (alphaNum <|> oneOf "_'")
  nobreaks
  return (x:l)


-- | Parsers corresponding to the six constructors of `Term`
constParse, strConstParse, varParse, unaryParse, binaryParse, ternaryParse :: Parser Term
constParse = Constant <$> intParse
strConstParse = StrConst <$> strParse
varParse = Var <$> vParse
unaryParse = Unary <$> tagParse
binaryParse = binaryParse' termParse
ternaryParse = ternaryParse' termParse'

binaryParse' :: Parser Term -> Parser Term
binaryParse' p = do
  c <- tagParse
  x <- whiteout $ p
  return $ Binary c x

ternaryParse' :: Parser (Term,Term) -> Parser Term
ternaryParse' p = do
  c <- tagParse
  (x,y) <- whiteout $ p
  return $ Ternary c x y
