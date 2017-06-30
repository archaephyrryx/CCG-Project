{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Test.Mode where

import           Control.Monad       (void, forM_)
import           Data.List
import           Data.Stringent
import           Util                (for, mono, one, verity, full, cond, afand, mapi, cdiv)
import           Widgets.Cast
import           Widgets.Core        hiding (Row, cast, label, wrap)
import qualified Widgets.Core        as Core (cast)
import           Widgets.Counter
import           Widgets.Group
import           Widgets.Links
import           Widgets.MultiSelect
import           Widgets.Phantom
import           Widgets.Radio
import           Widgets.Ranger
import           Widgets.Table
import           Widgets.Text
import           Widgets.Tower


data Mode = ModeA | ModeB | ModeC | ModeD
  deriving (Enum, Ord, Eq, Typeable, Show)

data Indexed = Indexed { index :: Int
                       , elment :: StaticText ()
                       }

instance Visible Indexed where
  visible = castAttr elment visible
  refresh = refresh . elment

instance Widget Indexed where
  widget = widget . elment

dynamate = dynamic . fill . expand

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
    inc <-  incre c

    countr <- preText c
    inc1 <- button c [ text := "+0" ]
    plus <- button c []


    babel <- table c []
    add <- preLink c
    ladder <- range c

    t <- table c []
    more <- incre' c [ text := "More" ]

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

            moder <- radio nav [ModeA .. ModeD] bMode show
            bMode <- stepper ModeA $ portents moder

            -- MODE A

            let bLast = bFinal caster

            ranged <- ranger rang bCurrent (pure 0) bLast (pure stringify)
            bCurrent <- restepper 0 (#) $ pageChanges


            cast <- genCast caster tab

            incs <- counter inc bPage (Static nItems)

            let eInc :: Event Int
                eInc = portents incs

                pageChanges :: Event (Int -> Int)
                pageChanges = priorityUnion [ min <$> bLast' <@ whenE (bThreshold) eInc
                                            , const <$> portents ranged]

            let nItems :: Int
                nItems = length . contents $ caster

                bTopReverseIndex :: Behavior Int
                bTopReverseIndex = ((nItems -).).(*) <$> bCurrent <*> bPage

                bThreshold :: Behavior Bool -- Have to go a page back
                bThreshold = ((<=) <$> bTopReverseIndex <*> bCurrent)

                -- what the last page would be, if we incremented the page sizeo
                --
                -- to find the pred, a great sacrifice was made
                bLast' :: Behavior Int
                bLast' = pred . cdiv nItems . succ <$> bPage

            let eRawPagesizeIncrement = portents incs
                bPageItems = (\cur pag -> (cur + 1) * pag)

            sink debug [ text :== (show.).(,) <$> bTopReverseIndex <*> bThreshold ]

            bPage <- stepper 1 eInc

            reactimate (refresh cast <$ eInc)

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

            -- MODE B
            bPlus <- softLink plus (const "+") 1
            bInc <- accumB 0 $ (+) <$> blood bPlus
            tInc <- liquidLink inc1 (pure (('+':).show)) bInc

            let eTInc :: Event Int
                eTInc = portents tInc
            bCount <- accumB 0 $ (+) <$> eTInc


            account <- rText countr (Dynamic (show <$> bCount)) (void eTInc)

            -- MODE C
            adder <- voidLink add "+"
            let eAdd = (length <$> bItems) <@ portents adder
            climb <- ranger ladder bCur (pure 0) bNext (pure stringify)
            let eChange = portents climb
            bCur <- stepper 0 eChange

            bNext <- accumB (-1) $ succ <$ eAdd
            reAdd <- mapEventIO (\n -> staticText (_tab babel) [ text := concat (replicate n (show n)) ]) eAdd

            let bThis = (\i xs -> if null xs then [] else [xs!!i]) <$> bCur <*> bItems
                eVis = (\x -> sink x [ visible :== elem x <$> bThis ] >> return x) <$> reAdd

            reVis <- execute eVis
            let eAll = flip (:) <$> bItems <@> reVis
            bItems <- stepper [] eAll

            hanoi <- erect babel bThis (cond full (grid 5 5 . map (\x -> [widget x])) (const (nullLayouts!!0)))

            -- MODE D

            let coin :: Int -> String -> IO Indexed
                coin i s = return . Indexed i =<< staticText (_tab t) [ text := s ]

                mint :: [IO Indexed]
                mint = mapi coin anthology

            coins <- liftIO $ sequence mint

            forM_ coins (\x -> sink (elment x) [ visible :== (>=(index x)) <$> bRange ])


            mores <- counter more bRange (Static nItems)

            let eMore = portents mores

            bRange <- stepper 0 eMore

            let bInRange = (\x -> filter ((<=x).index) coins) <$> bRange

            sink (_tab t) [ layout :== enboxed . dynamic . column 5 . map widget <$> bInRange ]




            let group1 = Group { _members = [ Item rang
                                            , Item incs
                                            , Item tab
                                            , Item actor]
                               , _layout = (\[a,b,c,d] -> dynamic . margin 10 $
                                 column 5 $
                                   [row 5 [widget a, widget b]
                                   , widget c
                                   , widget d])
                               }


                group2 = Group { _members = [Item account, Item tInc, Item bPlus]
                               , _layout = margin 10 . row 5 . map widget
                               }

                group3 = Group { _members = [Item adder, Item ladder, Item hanoi]
                               , _layout = margin 10 . column 5 . map widget
                               }
                group4 = Group { _members = [Item t, Item mores]
                               , _layout = margin 10 . column 5 . map widget
                               }


            ghost <- phantom [ aspect group1 ModeA Nothing
                             , aspect group2 ModeB Nothing
                             , aspect group3 ModeC Nothing
                             , aspect group4 ModeD Nothing
                             ] bMode
            grim <- reap content ghost
            reharvest grim (() <$ priorityUnion [ eInc, eMore ])
            relay (_tab babel) (() <$ eAdd)

            -- sink debug [ text :== bDebug ]
            liftIO $ set f [layout := margin 10 $ column 5 $ [widget nav, dynamate $ widget grim]]

    network <- compile networkDescription
    actuate network
