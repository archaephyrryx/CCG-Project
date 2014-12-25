module Main where

import Happstack.Server (nullConf, simpleHTTP, toResponse, ok, dir, seeOther, path)
import Control.Monad

main :: IO ()
main = simpleHTTP nullConf $ msum
    [ dir "hello" $ path $ \s -> ok $ "Hello, " ++ s
    , seeOther "/hello/World" "/hello/World"
    ]
