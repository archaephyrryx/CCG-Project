module Util.String where

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
