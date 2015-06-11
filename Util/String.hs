module Util.String where

import Util.Conditional
import Util.List

-- |'unquote' strips an optional leading single-quote
-- and an optional trailing single-quote from a string
unquote :: String -> String
unquote = full?.hstrip '\''.estrip '\''

-- |'unbrace' strips an optional leading open-square-bracket
-- and an optional trailing close-square-bracket from a string
unbrace :: String -> String
unbrace = full?.hstrip '['.estrip ']'

-- |Optionally strip a character from the head of a string
hstrip :: Char -> String -> String
hstrip c = full?.head.=c?.tail

-- |Optionally strip a character from the end of a string
estrip :: Char -> String -> String
estrip c = full?.last.=c?.init
