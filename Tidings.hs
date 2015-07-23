module Tidings
  ( Tidings
  , tidings
  , facts
  , rumors
  ) where

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
import Reactive.Banana.Combinators

data Tidings t a
  = T { facts  :: Behavior t a
      , rumors :: Event    t a
      }

tidings :: Behavior t a -> Event t a -> Tidings t a
tidings b e = T b (calm e)

instance Functor (Tidings t) where
  fmap f (T b e) = T (fmap f b) (fmap f e)

instance Applicative (Tidings t) where
  pure x = T (pure x) never
  f <*> x = uncurry ($) <$> pair f x

pair :: Tidings t a -> Tidings t b -> Tidings t (a,b)
pair (T bx ex) (T by ey) = T b e
  where
    b = (,) <$> bx <*> by
    x = flip (,) <$> by <@> ex
    y = (,) <$> bx <@> ey
    e = unionWith (\(x,_) (_,y) -> (x,y)) x y
