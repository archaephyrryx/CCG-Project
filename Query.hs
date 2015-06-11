{-# LANGUAGE RecordWildCards #-}
module Query where

import Control.Monad
import Control.Applicative
import Happstack.Server
import Site
import CCG
import Formal

getHParam :: (Hint h, ServerMonad m, MonadPlus m) => String -> m (Maybe h)
getHParam = (readH <$>) . optional . look

getLParams :: (ServerMonad m, MonadPlus m) => String -> (String -> a) -> m [a]
getLParams p f = optional . looks $ p >>= mmap f

parseCardFilter :: (ServerMonad m, MonadPlus m) => m Filter
parseCardFilter m = do
    powMin <- getHParam npl
    powMax <- getHParam xpl
    reqMin <- getHParam nrl
    reqMax <- getHParam xrl
    costMin <- getHParam ncl
    costMax <- getHParam xcl
    colors <- getLParams csl readC
    sets <- getLParams ssl readCS
    types <- getLParams tsl readT
    rarities <- getLParams rsl long
    return CardFilter{..}

parseDeckFilter :: (ServerMonad m, MonadPlus m) => m Filter
parseDeckFilter m = do
    colors <- getLParams csl readC
    sets <- getLParams ssl readCS
    types <- getLParams tsl readT
    rarities <- getLParams rsl long
    return DeckFilter{..}
