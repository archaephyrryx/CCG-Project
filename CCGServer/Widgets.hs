{-# LANGUAGE RecordWildCards #-}
module CCGServer.Widgets where

import Control.Applicative (Applicative)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Dynamic
import Data.Functor
import Data.IORef
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.String (fromString)
import Data.IxSet
import Data.Data ( Data, Typeable)
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Internal.FFI
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny.Attributes as UI
import qualified Graphics.UI.Threepenny.Elements   as UI
import qualified Graphics.UI.Threepenny.Events     as UI
import qualified Graphics.UI.Threepenny.Core       as UI
import Reactive.Threepenny
import Reactive.Threepenny hiding (onChange)
import Cards.Common.Hint


data MultiSelect a = MultiSelect
    { _elementMS   :: Element
    , _selectionMS :: Tidings [a]
    }

instance Widget (MultiSelect a) where getElement = _elementMS

-- | User changes to the current selection (possibly empty).
userSelections :: MultiSelect a -> Tidings [a]
userSelections = _selectionMS

-- | Create a 'ListBox'.
multiSelect :: Ord a
    => Behavior [a]               -- ^ list of items
    -> Behavior [a]               -- ^ selected items
    -> Behavior (a -> UI Element) -- ^ display for an item
    -> UI (MultiSelect a)
multiSelect bitems bsels bdisplay = do
    multi <- UI.select # set (attr "multiple") "1"

    -- animate output items
    element multi # sink items (map <$> bdisplay <*> bitems)

    -- animate output selection
    let bindices = indexify bitems
        indexify = ((Map.fromList . flip zip [0..]) <$>)
        bindex   = lookupIndices <$> bindices <*> bsels

        lookupIndices indices [] = []
        lookupIndices indices (sel:selt) = let rest = lookupIndices indices selt
                                           in maybe rest (:rest) (Map.lookup sel indices)

    element multi # sink selections bindex

    -- changing the display won't change the current selection
    -- eDisplay <- changes display
    -- sink listBox [ selection :== stepper (-1) $ bSelection <@ eDisplay ]

    -- user selection
    let bindices2 = Map.fromList . zip [0..] <$> bitems

        _selectionMS = tidings bsels $
            lookupIndices <$> bindices2 <@> selectionsChange multi
        _elementMS   = multi

    return MultiSelect{..}

selectionsChange :: Element -> Event [Int]
selectionsChange el = unsafeMapUI el (const $ get selections el) (UI.click el)

unsafeMapUI el f = unsafeMapIO (\a -> getWindow el >>= \w -> runUI w (f a))

items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ map (\i -> UI.option #+ [i]) i

selections :: Attr Element [Int]
selections = fromJQuerySelectedIndices from (JSON.toJSON)
    where
    from s = let JSON.Success x = JSON.fromJSON s in x

fromJQuerySelectedIndices :: (JSON.Value -> [Int]) -> ([Int] -> JSON.Value) -> Attr Element [Int]
fromJQuerySelectedIndices from to = mkReadWriteAttr get set
    where
    set v el = runFunction $ ffi "$(%1).val(%2)" el (to v)
    get   el = fmap from $ callFunction $ ffi "$(%1).val() || []" el

data Min a = Min
    { _elementMN   :: Element
    , _nuMN :: Tidings (Maybe a)
    }

data Max a = Max
    { _elementMX   :: Element
    , _nuMX :: Tidings (Maybe a)
    }

instance Widget (Min a) where getElement = _elementMN
instance Widget (Max a) where getElement = _elementMX

-- | User changes to the current selection (possibly empty).
userMin :: Min a -> Tidings (Maybe a)
userMin = _nuMN
userMax :: Max a -> Tidings (Maybe a)
userMax = _nuMX

minmax :: Hint a
    => Behavior (Maybe a) -- ^ Minimum value
    -> Behavior (Maybe a) -- ^ Maximum value
    -> Behavior (a -> String) -- ^ display for an item
    -> UI (Min a, Max a)
minmax bmin bmax bdisplay = do
    mini <-  UI.input # set (attr "type") "number" # set (attr "step") "1" # set (attr "placeholder") "Min" # set (attr "min") "0"
    maxi <-  UI.input # set (attr "type") "number" # set (attr "step") "1" # set (attr "placeholder") "Max" # set (attr "min") "0"

    -- animate output items
    element mini  # sink value ((maybe ("")) <$> bdisplay <*> bmin)
    element maxi  # sink (attr "min") ((maybe ("")) <$> bdisplay <*> bmin)
    element maxi  # sink value ((maybe ("")) <$> bdisplay <*> bmax)

    let _nuMN = tidings bmin $ readMaybeH <$> UI.valueChange mini
        _elementMN   = mini
        _nuMX = tidings bmax $ readMaybeH <$> UI.valueChange maxi
        _elementMX   = maxi
    return (Min{..}, Max{..})

readMaybeH "" = Nothing
readMaybeH x = Just (readH x)

data SearchBar a = SearchBar
    { _elementSB   :: Element
    , _matchesSB :: Tidings (Maybe a)
    }

instance Widget (SearchBar a) where getElement = _elementSB

searchBar :: (Ord a, Indexable a, Typeable a)
    => Behavior (IxSet a)         -- ^ list of items
    -> Behavior String            -- ^ partial search
    -> Behavior (a -> UI Element) -- ^ display for an item
    -> UI (SearchBar a)
searchBar reftab pstr rdisplay = do
    sbar <- UI.input # set type_ "search" # set placeholder "Search" # set value pstr
    sres <- UI.ul #. "dropdown-menu"

    let doSearch = toList . (reftab @+) . map (ravel :: String -> Nameword) . words

    element sres # sink children (map <$> rdisplay <*> doSearch pstr)

    -- user selection
        _matchesSB = tidings pstr $
            map <$> rdisplay <@> (doSearch <$> valueChange sbar)
        _elementSB   = UI.form #. "search-bar" #+ [element sbar, element sres]

    return SearchBar{..}

