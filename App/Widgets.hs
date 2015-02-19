{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module App.Widgets
        ( module App.Widgets
        , module App.Widgets.MultiSelect
        , module App.Widgets.Links
        , module App.Widgets.SearchBar
        , module App.Widgets.Ranger
        , module App.Widgets.MinMax
        ) where

import App.Core
import App.Widgets.Core
import App.Widgets.Links
import App.Widgets.SearchBar
import App.Widgets.MultiSelect
import App.Widgets.MinMax
import App.Widgets.Ranger


schildren = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i
