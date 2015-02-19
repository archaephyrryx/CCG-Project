{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DoRec #-}

module Test.Collector where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.IORef
import Data.List
import Data.List.Split
import Data.IxSet
import qualified Data.Map as Map
import Data.Map (Map)
import qualified	Graphics.UI.Threepenny 			as UI
import				Graphics.UI.Threepenny.Core
import App.Widgets
import App.Core.Helper

main :: IO ()
main = do
	startGUI defaultConfig
		{ tpPort	= Just 10000
		, tpStatic	= Just "../wwwroot/"
		} setup

numers = ["Zero","One","Two","Three","Four","Five","Six","Seven","Eight","Nine"]
lettrs = ['A'..'J']

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title "Test (Collector)"

    -- Fixed number

    let values = [0..9]
        biteSize = 5
        bits = length values
        bites = bits`cdiv`biteSize

    rec range <- ranger bThis bFirst bLast (pure (string.show.succ)) 
        let tRanger = userLoc range
            eRanger = rumors tRanger
            bRanger = facts tRanger
            bFirst = pure 0
            bLast = (pred) <$> (pure bites)
        bThis <- stepper 0 $ eRanger

    rec range' <- ranger bThis' bFirst' bLast' (pure (string.show.succ)) 
        let tRanger' = userLoc range'
            eRanger' = rumors tRanger'
            bRanger' = facts tRanger'
            bFirst' = pure 0
            bLast' = (pred) <$> (pure bites)
        bThis' <- stepper 0 $ eRanger'

    let chunks = chunksOf biteSize values
        bValues = map (\x -> (!!x) <$> ((chunks!!) <$> bThis)) (enumFromTo 0 (biteSize-1))

    softs <- sequence (zipWith (softLink) (map (numers!!) values) (values))
    liquids <- sequence (zipWith (liquidLink) (replicate biteSize (numers!!)) (bValues))
    
    let chunks' = chunksOf biteSize softs
        bSofts = (chunks'!!) <$> bThis'

    let eSofts = (map (rumors.tideLink) softs)
    let eLiquids = (map (rumors.tideLink) liquids)

    bFoo <- stepper (-1) $ head <$> unions (eSofts++(map ((-1) <$) eLiquids))
    bBar <- stepper (-1) $ head <$> unions (eLiquids++(map ((-1) <$) eSofts))

    val <- UI.h1
    element val # sink UI.text ((cond (>=0) ((:[]).(lettrs!!)) (const "")) <$> bFoo)
    val' <- UI.h1
    element val' # sink UI.text ((cond (>=0) ((:[]).(lettrs!!)) (const "")) <$> bBar)

    softBox <- UI.div
    liquidBox <- UI.div

    element softBox # sink schildren (map (\x -> row [element x]) <$> bSofts)
    element liquidBox # sink schildren (map (\x -> row [element x]) <$> (pure liquids))

    getBody window #+ [ column ([ row [element val], row [element range'] ]++[element softBox])
                      , column ([ row [element val'], row [element range] ]++[element liquidBox])
                      ]
