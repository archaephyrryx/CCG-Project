{-# LANGUAGE RecordWildCards #-} 

module CCG.Cards.Parser (
      parsage
    , unquote
    , unbrace
    , ocrBlock
    , headline
    , makeline
    , fields
    , findBreakPoint
    , keys
    ) where

import CCG.Cards
import CCG.Cards.Common
import Control.Applicative
import qualified CCG.Cards.Parser.Fields as F (OrderedField(..), FieldWrapper(..), getField, wrapFields)
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Function (on)
import Util


-- |Parser to convert the full text of an OCR file into a list of Cards
ocrBlock :: String -- ^ Unprocessed contents of an OCR file
         -> [Card] -- ^ List of Cards from OCR text string
ocrBlock str = let (h:xs) = lines str
                   header = headline h
                   line = makeline header
               in map line xs

-- | Line-to-fields string-splitter based on a tab-delimited OCR with
-- each field possibly wrapped in single quotes (independent of other
-- fields)
fieldSplit :: String -> [String]
fieldSplit = map unquote . split (dropInitBlank . dropDelims $ oneOf "\t")

-- | Given an opening character and closing character to search for,
-- returns the indices of the spanning delimters, 
between :: Char -> Char -> String -> (Int,Int)
between o c s = let open = elemIndices o s
                    fopen = headDef 0 open
                    close = reverse $ elemIndices c s
                    lclose = if (not.null$open) && null close
                                then findBreakPoint (drop (fopen+1) s)
                                else succ . headDef (-1) $ close
                in (fopen,lclose)

-- | Give
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
                cardParse (F.wrapFields (F.OrderedField l) f)

cardParse :: F.FieldWrapper -> Card
cardParse fw =
        let key' = keys key
            text = ravel text'
            set = long set'
            num = readN num'
            rar = readR rar'
        in case F.typ fw of
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
                       preqs' = (,) <$> (fcond ((:) <$> ((,) <$> readC.F.col1 <*> readH.F.preq1) <*>) (F.preq2.="") (const []) (((:[]).).(,) <$> readC.F.col2 <*> readH.F.preq2)) <*> (readH.F.opreq) $ fw
                                  --then ([(readC . F.col1 $ fw, readH . F.preq1 $ fw)], readH F.opreq)
                                  --else ([(readC . F.col1 $ fw, readH . F.preq1 $ fw), (readC F.col2, readH F.preq2)], readH F.opreq)
                   in (Problem nam set num rar poi' key' preqs' text)
  where
    nam = F.nam fw
    key = F.key fw
    text' = F.text' fw
    set' = F.set' fw
    num' = F.num' fw
    rar' = F.rar' fw
    boo = F.boo fw
    pow = F.pow fw
    col = F.col fw
    cos = F.cos fw
    req = F.req fw
    poi = F.poi fw



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
