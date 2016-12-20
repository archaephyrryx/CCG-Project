{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Test.Mode where

import           Control.Monad       (void, forM_)
import           Data.List
import           Data.Stringent
import           Util                (for, mono, one, verity, full, cond, afand, mapi)
import           Widgets.Cast
import           Widgets.Core        hiding (Row, cast, label, wrap)
import qualified Widgets.Core        as Core (cast)
import           Widgets.Group
import           Widgets.Links
import           Widgets.MultiSelect
import           Widgets.Phantom
import           Widgets.Radio
import           Widgets.Ranger
import           Widgets.Table
import           Widgets.Text
import           Widgets.Tower


data Mode = ModeA | ModeB
  deriving (Enum, Ord, Eq, Typeable, Show)

data Indexed = Indexed { index :: Int
                       , elment :: StaticText ()
                       }

instance Visible Indexed where
  visible = castAttr elment visible
  refresh = refresh . elment

instance Widget Indexed where
  widget = widget . elment

main :: IO ()
main = start test

anthology :: [String]
anthology =
  [ "A Midsummer Night's Dream"
  , "A Comedy of Errors"
  , "Much Ado About Nothing"
  , "King Lear"
  , "Richard III"
  , "Romeo and Juliet"
  , "Macbeth"
  , "Hamlet"
  , "The Tempest"
  , "Twelfth Night"
    , "Taming of the Shrew"
    , "Merchant of Venice"
    , "Othello"
  ]

test :: IO ()
test = do
    f <- frame [text := "Test"]

    nav <- table f []
    content <- table f []
    let c = _tab content

    rang <- range c
    tab <- table c []
    act <- staticText c []
    debug <- staticText c []
    inc <- preLink c

    let networkDescription :: MomentIO ()
        networkDescription = mdo
            let bAnthology = pure anthology
                caster = Case { contents = anthology
                              , bPagesize = bPage
                              , current = tidings bCurrent $ portents ranged
                              , format = Format { label = Static stringify
                                                , wrap = Static (\s l -> Row [Item l])
                                                , collect = Static id
                                                }
                              }

            moder <- radio nav [ModeA .. ModeB] bMode show
            bMode <- stepper ModeA $ portents moder

            -- MODE A

            let bLast = bFinal caster

            ranged <- ranger rang bCurrent (pure 0) bLast (pure stringify)
            bCurrent <- restepper 0 (#) $ pageChanges

            cast <- genCast caster tab

            incs <- voidLink inc "+"

            let eInc :: Event ()
                eInc = portents incs

                pageChanges :: Event (Int -> Int)
                pageChanges = priorityUnion [ pred <$ whenE (bThreshold) eInc
                                            , const <$> portents ranged]

            let nItems :: Int
                nItems = length . contents $ caster

                bOnLastPage :: Behavior Bool -- Are we on the last page
                bOnLastPage = (==) <$> bCurrent <*> bLast
                bLastPageSize :: Behavior Int -- How many items are on the last page
                bLastPageSize = ((flip subtract nItems .) . (*)) <$> bLast <*> bPage

                bThreshold :: Behavior Bool -- Have to go a page back
                bThreshold = afand bOnLastPage ((<=) <$> bLastPageSize <*> bLast)

            let eRawPagesizeIncrement = portents incs
                bPageItems = (\cur pag -> (cur + 1) * pag)

            let bCanInc = ((<nItems) <$> bPage)
                eValidIncrement = whenE bCanInc eInc

            sink debug [ text :== (show.).(,) <$> bThreshold <*> bLastPageSize ]

            bPage <- restepper 4 (#) $ (+1) <$ eValidIncrement

            reactimate (refresh cast <$ trueInc)

            let eActua = priorityUnion ([0 <$ portents incs, portents cast])
            bClicked <- stepper (-1) $ eActua
            reactimate (refit (_tab tab) <$ portents ranged)
            actor <- rText act (Dynamic (stringify <$> bClicked)) (void eActua)

            let
                --bDebug :: Behavior String
                --bDebug = show . map ((map _crux).unpack) <$> bRows
                bRows :: Behavior [Row]
                bRows = _rows . _elem . _cast $ cast
                bShows :: [Behavior Bool]
                bShows = for [0..] (\x -> (==x) <$> bCurrent)
                unpack :: Row -> [Link Int]
                unpack = \r -> extract $ _items r
                  where
                    extract :: [Item] -> [Link Int]
                    extract [] = []
                    extract ((Item x):t) = case Core.cast x of
                                             Just (x :: Link Int) -> x:(extract t)
                                             Nothing -> extract t

                bUnpacked :: Behavior [Link Int]
                bUnpacked = concatMap unpack <$> bRows

            let group1 = Group { _members = [ Item rang
                                            , Item incs
                                            , Item tab
                                            , Item actor]
                               , _layout = (\[a,b,c,d] -> margin 10 $
                                 column 5 $
                                   [row 5 [widget a, widget b]
                                   , dynamic $ widget c
                                   , widget d])
                               }


            ghost <- phantom [ aspect group1 ModeA Nothing
                             , aspect nullGroup ModeB Nothing
                             ] bMode
            grim <- reap content ghost
            reharvest grim eValidIncrement

            -- sink debug [ text :== bDebug ]
            liftIO $ set f [layout := dynamic $ margin 10 $ column 5 $ [widget nav, fill $ dynamic $ widget grim]]

    network <- compile networkDescription
    actuate network
