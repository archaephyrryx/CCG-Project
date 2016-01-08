module Util.Advanced where

import Util.Tuple.Apply
import Util.Conditional

-- |Like 'cond', but with a pre-application after the test
-- >  precond h p f g = cond p (f.h) (g.h)
precond :: (a -> b) -> (a -> Bool) -> (b -> c) -> (b -> c) -> (a -> c)
precond = fcond . flip (.)

-- |Like 'cond', only the conditional function is derived by applying a
-- generator function to a conditional parameter
-- >  fcond h p f g = cond p (h f) (h g)
fcond :: (x -> (a -> b)) -> (a -> Bool) -> x -> x -> (a -> b)
fcond = (curry.).flip((.).uncurry.cond).tmup

-- |A case of 'fcond' with 'const' as the generator, returning one of
-- two values depending on the result of the test
consd :: (a -> Bool) -> b -> b -> (a -> b)
-- >  consd p x y a = if p a then x else y
consd = fcond const

condense :: (a -> Bool) -> (a -> b) -> (a -> c) -> [a] -> ([b],[c])
-- > condense p f g [] = ([],[])
-- > condense p f g (x:xs) = if_ (p x) ((f x:)$<) (>$(g x:)) $ xs
condense = (((`foldr`([], [])).).).flip flip ((flip (>$).(:)).).((.).).(.((($<).(:)).)).cond

