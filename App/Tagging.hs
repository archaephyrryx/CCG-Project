{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RecursiveDo #-} 

module App.Tagging where
-------------------------------------------------
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Data (Data, Typeable)
import Data.IORef
import Data.IxSet
import Data.Map (Map)
import qualified Data.Map as Map
-------------------------------------------------
import Control.Applicative
import Control.Monad
--------------------------------------------------
import API.Database
import API.TagState
--------------------------------------------------
import CCG
--------------------------------------------------
import App.Core
--------------------------------------------------
import qualified Graphics.UI.Threepenny          as UI
import qualified Graphics.UI.Threepenny.Core     as UI
import qualified Graphics.UI.Threepenny.Elements as UI
import Graphics.UI.Threepenny.Core

setup :: Window -> UI ()
setup window = void $ do
    return window # UI.set UI.title appfname
    addStyleSheet window "style.css"

    -- Tagging Only
    elPlus    <- UI.button # settext "+"
    elMinu    <- UI.button # settext "-"
    elSave    <- UI.button # settext "Save Changes"
    elDrop    <- UI.button # settext "Discard Changes"
    elMode    <- UI.button # settext "View"
    elBlank   <- UI.input  # settext  ""
    result    <- UI.span

    inputs <- liftIO $ newIORef []
    mode   <- liftIO $ newIORef View
    edited <- liftIO $ newIORef False
    dats   <- liftIO $ newIORef example

	let key = fst example


	-- Layout and display
	let
		displayAll = void $ do
			xs <- mapM (get value) =<< liftIO (readIORef inputs)
			element result # set text (":\t"++((intercalate ", ").reverse $ xs))
		
		redoLayout :: UI ()
		redoLayout = void $ do
			emode <- liftIO (readIORef mode)
			edits <- liftIO (readIORef edited)
			let canedit = canEdit emode
			layout <- mkLayout canedit =<< liftIO (readIORef inputs)
			getBody window # set children [layout]
			displayAll
		
		mkLayout :: Bool -> [Element] -> UI Element
		mkLayout e xs = column $
			([row [element elMode], UI.hr]++
			mkLine e xs key ++
			[UI.hr, row [element elSave, element elDrop]])
			
		mkLine :: Bool -> [Element] -> String -> [UI Element]
		mkLine e xs s = [row [UI.span # set text s]] ++ rebutrows
			where
				rebutrows = if e
								then concatMap (\x -> [row x]) (reverse xs')
								else [row [element result]]
				xs' = (ebut (head xs)):(map (\x -> [element x]) (tail xs))
				ebut y = (element y):(if e then [element elPlus, element elMinu] else [])

	-- Mode switching
	let 
		toggleMode :: UI ()
		toggleMode = do
			liftIO $ modifyIORef mode (toggle)

		toggler = void $ do
			emode <- liftIO (readIORef mode)
			element elMode # set text (show.toggle$emode)

	-- Save/Discard modifications
	
		saveChanges :: UI ()
		saveChanges = do
			tags <- mapM (get value) =<< liftIO (readIORef inputs)
			liftIO $ modifyIORef dats (\(y,x) -> (y, tags))

		discardChanges :: UI ()
		discardChanges = do
			liftIO $ modifyIORef inputs (const [])
			vdat <- liftIO (readIORef dats)
			mapM_ addInputVal (snd vdat)
			when (null (snd vdat)) (addInput)
	
	-- Add/Remove Fields
		addInputVal :: String -> UI ()
		addInputVal s = do
			elInput <- UI.input # set value s
			on (domEvent "livechange") elInput $ \_ -> (liftIO $ modifyIORef edited (const True)) >> displayAll
			liftIO $ modifyIORef inputs (elInput:)
			
		addInput :: UI ()
		addInput = addInputVal ""

		dropInput :: UI ()
		dropInput = do
			elInput <- UI.input # set value ""
			on (domEvent "livechange") elInput $ \_ -> (liftIO $ modifyIORef edited (const True)) >> displayAll
			liftIO $ modifyIORef inputs ((cond null (elInput:) id).(drop 1))

	-- Click behaviors

	on UI.click elPlus $ const $ do
		liftIO $ modifyIORef edited (const True)
		addInput
		redoLayout
	on UI.click elMinu $ const $ do
		liftIO $ modifyIORef edited (const True)
		dropInput
		redoLayout
	on UI.click elMode $ const $ do
		toggleMode
		toggler
		redoLayout
	on UI.click elSave $ const $ do
		saveChanges
		liftIO $ modifyIORef edited (const False)
		redoLayout
	on UI.click elDrop $ const $ do
		discardChanges
		liftIO $ modifyIORef edited (const False)
		redoLayout
	toggleMode >> toggler >> discardChanges >> redoLayout

-- Mode toggle

data Mode = View | Edit
	deriving (Read, Show, Eq)

toggle :: Mode -> Mode
toggle View = Edit
toggle Edit = View

canEdit :: Mode -> Bool
canEdit View = False
canEdit Edit = True

example = ("Cat",[])
