{-# LANGUAGE FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Cards.Common.Hint where

import Cards.Common
import Data.Maybe

class Hint a where
	val :: a -> Int
	unval :: Int -> a
	readH :: String -> a
	readH = unval.read 

readMaybeH :: (Hint a) => String -> Maybe a
readMaybeH "" = Nothing
readMaybeH x = Just (readH x)


plus :: (Hint a) => a -> a -> a
plus = (unval.).(.val).(+).val

minus :: (Hint a) => a -> a -> a
minus = (unval.).(.val).(-).val

inc :: (Hint a) => Int -> a -> a
inc = (unval.).(.val).(+)

dec :: (Hint a) => Int -> a -> a
dec = (unval.).(.val).(+).negate

instance Hint Int where
    val = id
    unval = id
    readH = read

instance Hint Power where
	val (Power x) = x
	unval x = (Power x)

instance Hint Cost where
	val (Cost x) = x
	unval x = (Cost x)

instance Hint Req where
	val (Req x) = x
	unval x = (Req x)

instance Hint Points where
	val (Points x) = x
	unval x = (Points x)
