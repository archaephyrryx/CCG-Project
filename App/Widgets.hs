{-# LANGUAGE RecordWildCards #-}
module App.Widgets where

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
import Data.List
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
import Cards.Common.Stringe
import Cards.Common.Abbrev
import Database
import App.Core.Helper

-- * MultiSelect * --
-- |A customized version of ListBox that allows multiple elements to be
-- selected and provides its own clear-button
data MultiSelect a = MultiSelect
    { _elementMS   :: Element
    , _selectionMS :: Tidings [a]
    }

instance Widget (MultiSelect a) where getElement = _elementMS

-- | User changes to the current selection (possibly empty).
userSelections :: MultiSelect a -> Tidings [a]
userSelections = _selectionMS

-- | Create a 'MultiSelect'.
multiSelect :: Ord a
    => String         -- ^ Title for the list
    -> Bool           -- ^ Multiple or not
    -> Behavior [a]   -- ^ list of items
    -> Behavior [a]   -- ^ selected items
    -> Behavior (a -> UI Element) -- ^ display for an item
    -> UI (MultiSelect a)
multiSelect hed m bitems bsels bdisplay = do
    multi <- UI.select
    clearbut <- UI.button #. "clear-btn" # settext "clear"
    wrapper <- UI.table #+ [ UI.tr #+ [ UI.th #+ [UI.bold #+ [UI.string hed]]
                                      , UI.th #+ [element clearbut]
                                      ]
                            , UI.tr #+ [element multi]
                            ]
    -- animate output items
    element multi # set (attr "multiple") (if m then "1" else "0")
    element multi # sink items (map <$> bdisplay <*> bitems)

    -- animate output selection
    let bindices = indexify bitems
        indexify = ((Map.fromList . flip zip [0..]) <$>)
        bsindices   = lookupIndices <$> bindices <*> bsels

        lookupIndices indices [] = []
        lookupIndices indices (sel:selt) = let rest = lookupIndices indices selt
                                           in maybe rest (:rest) (Map.lookup sel indices)

    element multi # sink selections bsindices

    -- changing the display won't change the current selection
    -- eDisplay <- changes display
    -- sink listBox [ selection :== stepper (-1) $ bSelection <@ eDisplay ]

    -- user selection
    let bindices2 = Map.fromList . zip [0..] <$> bitems
        _selectionMS = tidings bsels $
            lookupIndices <$> bindices2 <@> (unionWith const (selClear clearbut) (selectionsChange multi))
        _elementMS   = wrapper
    return MultiSelect{..}

selectionsChange :: Element -> Event [Int]
selectionsChange el = unsafeMapUI el (const $ get selections el) (UI.click el)

selClear :: Element -> Event [Int]
selClear el = unsafeMapUI el (const $ return []) (UI.click el)

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



-- * Min and Max * --
-- |A "minimum value" numeric input field
data Min a = Min
    { _elementMN   :: Element
    , _nuMN :: Tidings (Maybe a)
    }

-- |A "maximum value" numeric input field
data Max a = Max
    { _elementMX   :: Element
    , _nuMX :: Tidings (Maybe a)
    }

instance Widget (Min a) where getElement = _elementMN
instance Widget (Max a) where getElement = _elementMX

-- | User changes to the current values (possibly empty).
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

-- * SearchBar * --
-- |A search-bar that performs auto-completed lookups into an IxSet
data SearchBar a = SearchBar
    { _elementSB :: Element
    , _searchsSB :: Tidings String
    , _matchesSB :: Tidings ([a])
    }

instance Widget (SearchBar a) where getElement = _elementSB

userSearch :: SearchBar a -> Tidings (String)
userSearch = _searchsSB

searchBar :: (Ord a, Indexable a, Typeable a)
    => Behavior (IxSet a)         -- ^ list of items
    -> Behavior String            -- ^ partial search
    -> Behavior (a -> UI Element) -- ^ display for an item
    -> UI (SearchBar a)
searchBar reftab pstr rdisplay = do
    sbar <- UI.input
    sres <- UI.ul
    scomb <- UI.form #. "search-bar" #+ [element sbar, element sres]
    let
        doSearch rt ps = toList (rt @+ wds)
            where
              wds :: [Nameword]
              wds = map ravel . words $ ps

    element sbar # set UI.type_ "search"
    element sbar # set (attr "placeholder") "Search"
    element sbar # sink UI.value pstr

    element sres #. "dropdown-menu"
    element sres # sink qmatches (map <$> rdisplay <*> (doSearch <$> reftab <*> pstr))

    -- user selection
    let
        _matchesSB = tidings (doSearch <$> reftab <*> pstr) $
            doSearch <$> reftab <@> UI.valueChange sbar
        _searchsSB = tidings pstr $ UI.valueChange sbar
        _elementSB   = scomb

    return SearchBar{..}

qmatches = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ map (\i -> UI.li #+ [i]) i



-- * SoftLink * --
-- |A hybrid Link/Button, which can be made into either with CSS rules.
--  The SoftLink stores a single value, which can be hooked to a
--  value-computed action.
--  Useful in contexts where the desired action is deterministic,
--  static, and completely changes the context of the UI, but where
--  the content is rendered instead of cached, making links impractical.
data SoftLink a = SoftLink
  { _elementSL :: Element
  , _cruxSL :: a
  }

instance Widget (SoftLink a) where getElement = _elementSL

softLink :: String -- Value to display
         -> a -- Value to hold
         -> UI (SoftLink a)
softLink dval grist = do
    link <- UI.button #. "softlink" # UI.set UI.text dval

    let _elementSL = link
        _cruxSL = grist
    return SoftLink{..}

getCrux :: SoftLink a -> a
getCrux = _cruxSL

-- |An infix-able linking function that associates a SoftLink to its
-- value-computed action
linksTo :: SoftLink a -> (a -> UI ()) -> UI ()
sl`linksTo`f = on UI.click (getElement sl) $ \_ -> f (getCrux sl)

-- | Mutable-content softlink
data LiquidLink a = LiquidLink
  { _elementLL :: Element
  , _fluxLL :: Behavior a
  }

instance Widget (LiquidLink a) where getElement = _elementLL

liquidLink :: (a -> String) -- Value to display
           -> Behavior a -- Value to hold
           -> UI (LiquidLink a)
liquidLink fdval fluid = do
    link <- UI.button #. "softlink"
    element link # sink text (fdval <$> fluid)

    let _elementLL = link
        _fluxLL = fluid
    return LiquidLink{..}

getFlux :: LiquidLink a -> UI a
getFlux = currentValue . _fluxLL

-- |An infix-able linking function that associates a LiquidLink to its
-- dynamic value-computed action
sinksTo :: LiquidLink a -> (a -> UI ()) -> UI ()
ll`sinksTo`f = on UI.click (getElement ll) $ \_ -> (f =<< getFlux ll)
