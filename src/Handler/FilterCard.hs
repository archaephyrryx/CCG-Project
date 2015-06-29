{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Handler.FilterCard (parseCardFilter) where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Data.Map (Map, lookup)
---------------------------------------------
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)
import           Control.Applicative
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Data.Maybe
import           Control.Monad
import           Heist
import qualified Heist.Interpreted as I
---------------------------------------------
import           Application
import           CCG hiding (render)
import           Util.List
import           Util.Conditional
import           Formal
import           API.Filter

getHParam :: Hint h => Params -> String -> Maybe h
getHParam m p = readMaybeH $ (B.unpack . head)?/(lookup (B.pack p) m)

getLParams :: Params -> String -> [a] -> [a]
getLParams m p vs = vs !@ (mmap (read . B.unpack :: ByteString -> Int) $ lookup (B.pack p) m)

parseCardFilter :: Params -> Filter
parseCardFilter m = let powMin = m`getHParam`npl
                        powMax = m`getHParam`xpl
                        reqMin = m`getHParam`nrl
                        reqMax = m`getHParam`xrl
                        costMin = m`getHParam`ncl
                        costMax = m`getHParam`xcl
                        colors = getLParams m csl colorValues
                        sets = getLParams m ssl setValues
                        types = getLParams m tsl typeValues
                        rarities = getLParams m rsl rarityValues
                    in CardFilter{..}
