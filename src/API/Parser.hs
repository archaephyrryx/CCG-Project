module API.Parser where

import CCG
import Util
import Util.List
import Data.List.Split
import Data.Char

parseTexts :: UniCard a => a -> [String]
parseTexts = precond (utype.=TMane) (unravel.utext) bitext one

bitext :: String -> [String]
bitext s = [front, back]
    where parses = (splits (cases " Back: ") s)
          (infront:back:[]) = head . filter ((==2).length) $ parses
          front = tail . snd . break (isSpace) $ infront

splits :: [String] -> String -> [[String]]
splits bs s = for bs (flip splitOn s)

cases :: String -> [String]
cases x = [x, map toUpper x]

pbreak :: String -> [String]
pbreak = splitOn "<P>"

pbreaks :: [String] -> [[String]]
pbreaks = map pbreak
