{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Test.Mode where

import           Control.Monad       (void, forM_)
import           Data.Bits
import           Data.List
import           Data.Stringent
import           Util                (for, mono, one, verity, full, cond, afand, mapi)
import           Widgets.Core        hiding (Row, cast, label, wrap, elastic)
import qualified Widgets.Core        as Core (cast)
import           Widgets.Counter
import           Widgets.Group
import           Widgets.Links
import           Widgets.Phantom
import           Widgets.Radio
import           Widgets.Table


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

nItems :: Int
nItems = length anthology

test :: IO ()
test = do
    vbox <- boxSizerCreate wxVERTICAL
    hbox <- boxSizerCreate wxHORIZONTAL

    f <- frame [text := "Test"]
    let a = f
    -- a <- panel f []

    t <- table a []
    more <- incre' a [ text := "More" ]
    windowSetSizer a vbox

    sizerAddWindow vbox (_tab t)    1 (wxALL) 5 ptrNull
    sizerAddWindow vbox (_inc more) 0 (wxALL) 5 ptrNull
    -- set a [ layout := sizer vbox ]
    -- windowSetSizer f hbox
    -- sizerAddWindow hbox a 1 (wxALL .|. wxEXPAND) 5 ptrNull
    -- set f [ layout := dynamic $ widget a ]
    frameCenter f

    let networkDescription :: MomentIO ()
        networkDescription = mdo
            let relay = do
                windowReLayout f
                windowReLayout a

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

            sink t [ layout :== dynamic . column 5 . map (widget) <$> bInRange ]

            -- reactimate (relay <$ eMore)

    windowReLayout f
    windowReLayout a
    network <- compile networkDescription
    actuate network
