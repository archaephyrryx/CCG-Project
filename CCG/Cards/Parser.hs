{-# LANGUAGE RecordWildCards #-} 

module CCG.Cards.Parser (parsage, unquote, unbrace, ocrBlock) where

import CCG.Cards
import CCG.Cards.Common
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Function (on)

newtype OrderedField = OrderedField { flds :: [String] }

getField :: String -> OrderedField -> [([String] -> String)]
getField x y = map (flip (!!)) (elemIndices x (flds y))

neck = (!!1)
body = (!!2)

data FieldWrapper =
     FieldWrapper
     { nam   :: String
     , set'  :: String
     , num'  :: String
     , rar'  :: String
     , typ   :: String
     , key   :: String
     , col   :: String
     , cos   :: String
     , req   :: String
     , pow   :: String
     , boo   :: String
     , poi   :: String
     , preq1 :: String
     , col1  :: String
     , preq2 :: String
     , col2  :: String
     , opreq :: String
     , text' :: String
     }
     deriving (Show)

wrapFields :: OrderedField -> [String] -> FieldWrapper
wrapFields ordfs fs =
  let nam   = (head $ getField "Name" ordfs) $ fs
      set'  = (head $ getField "Set" ordfs) $ fs
      num'  = (head $ getField "Number" ordfs) $ fs
      rar'  = (head $ getField "Rarity" ordfs) $ fs
      typ   = (head $ getField "Type" ordfs) $ fs
      key   = (head $ getField "Keywords" ordfs) $ fs
      col   = (head $ getField "Color" ordfs) $ fs
      cos   = (head $ getField "Cost" ordfs) $ fs
      req   = (head $ getField "Req" ordfs) $ fs
      pow   = (head $ getField "Power" ordfs) $ fs
      boo   = (head $ getField "Boosted" ordfs) $ fs
      poi   = (head $ getField "Points" ordfs) $ fs
      preq1 = (head $ getField "Problem Req" ordfs) $ fs
      col1  = (neck $ getField "Color" ordfs) $ fs
      preq2 = (neck $ getField "Problem Req" ordfs) $ fs
      col2  = (body $ getField "Color" ordfs) $ fs
      opreq = (head $ getField "Opponent Req" ordfs) $ fs
      text' = (head $ getField "Text" ordfs) $ fs
  in FieldWrapper{..}

ocrBlock :: String -> [Card]
ocrBlock str = let (h:xs) = lines str
                   header = headline h
                   line = makeline header
               in map line xs

fieldSplit :: String -> [String]
fieldSplit = map unquote . split (dropInitBlank . dropDelims $ oneOf "\t")

unquote :: String -> String
unquote x | null x = x
          | head x == '\'' && last x == '\'' = init.tail $ x
          | last x == '\'' = tail $ x
          | head x == '\'' = init $ x
          | otherwise = x

unbrace :: String -> String
unbrace x | null x = x
          | head x == '[' && last x == ']' = init.tail $ x
          | head x == '[' = tail $ x
          | last x == ']' = init $ x
          | otherwise = x

headDef :: a -> [a] -> a
headDef x [] = x
headDef _ (x:_) = x

between :: Char -> Char -> String -> (Int,Int)
between o c s = let open = elemIndices o s
                    fopen = headDef 0 open
                    close = reverse $ elemIndices c s
                    lclose = if (not.null$open) && null close
                                then findBreakPoint (drop (fopen+1) s)
                                else succ . headDef (-1) $ close
                in (fopen,lclose)

findBreakPoint :: String -> Int
findBreakPoint "" = 0
findBreakPoint s = let ws = split (condense . dropDelims $ oneOf " [],") s
                       ws' = split (condense $ oneOf " [],") s
                       li = last [i | i<-[1..(length ws)], uncurry (isInfixOf `on` unwords) (splitAt i ws)]
                       lw = ws !! (li - 1)
                       si = headDef 0 (elemIndices lw ws')
                       ss = take (si + 1) ws'
                       l = sum . map length $ ss
                   in (l+1)



splitOn' :: Eq a => [a] -> [a] -> [[a]]
splitOn' str = split (dropBlanks . dropDelims . onSublist $ str) 

headline :: String -> [String]
headline hl = fieldSplit hl

makeline :: [String] -> String -> Card
makeline labs = parseLine.fields labs

fields :: [String] -> String -> [(String,String)]
fields ls str = zip ls (fieldSplit str)

parseLine :: [(String,String)] -> Card
parseLine lf = let (l,f) = unzip lf in
                cardParse (wrapFields (OrderedField l) f)

cardParse :: FieldWrapper -> Card
cardParse fw@FieldWrapper{..} =
        let key' = keys key
            text = ravel text'
            set = long set'
            num = readN num'
            rar = readR rar'
        in case typ of
            "Mane"
                -> let boo' = readH boo
                       pow' = readH pow
                       col' = readC col
                   in (Mane nam set num rar key' col' pow' boo' text)
            "Friend"
                -> let pow' = readH pow
                       col' = readC col
                       cos' = readH cos
                       req' = readH req
                   in (Friend nam set num rar key' col' cos' req' pow' text)
            "Resource"
                -> let pow' = readH pow
                       col' = readC col
                       cos' = readH cos
                       req' = readH req
                   in (Resource nam set num rar key' col' cos' req' pow' text)
            "Event"
                -> let pow' = readH pow
                       col' = readC col
                       cos' = readH cos
                       req' = readH req
                   in (Event nam set num rar key' col' cos' req' pow' text)
            "Troublemaker"
                -> let poi' = readH poi
                       pow' = readH pow
                   in (Troublemaker nam set num rar key' pow' poi' text)
            "Problem"
                -> let poi' = readH poi
                       preqs' = if preq2 == ""
                                  then ([(readC col1, readH preq1)], readH opreq)
                                  else ([(readC col1, readH preq1), (readC col2, readH preq2)], readH opreq)
                   in (Problem nam set num rar poi' key' preqs' text)


keys :: String -> Keywords
keys str | null str = nokeys
         | isOneSided str = onesided str
         | isTwoSided str = twosided str
         | isOneSided (takeWhile (/=';') str) = keys (takeWhile (/=';') str)
         | otherwise = error "Could not identify format of keyword string"
    where
        isOneSided = all (`elem`(concat [['A'..'Z'],['a'..'z'],[' ',',','[',']']]))
        isTwoSided = all (`elem`(concat [['A'..'Z'],['a'..'z'],[' ',',','/','[',']']]))
        nokeys = []

onesided :: String -> Keywords
onesided str = let primaries = map unbrace (splitOn' "] [" primary)
                   secondaries = map (dropWhile isSpace) $ splitOn' "," secondary
               in (map ravel ((map (\x -> "["++x++"]") primaries)++(secondaries\\primaries)))
    where
        (fopen,lclose) = between '[' ']' str
        primary = drop fopen . take lclose $ str
        secondary = dropWhile isSpace . drop lclose $ str

twosided :: String -> Keywords
twosided str = let primaries = map unbrace (splitOn' "]/[" primary)
                   secondaries = map (map (dropWhile isSpace) . splitOn' ",") (splitOn' "/" secondary)
               in (map ravel . concat $ (zipWith (\x xs -> x:xs) (map (\x -> "["++x++"]") primaries) (zipWith delete primaries secondaries)))
    where
        (fopen,lclose) = between '[' ']' str
        primary = drop fopen . take lclose $ str
        secondary = dropWhile isSpace . drop lclose $ str

parsage = Set.fromList.ocrBlock
