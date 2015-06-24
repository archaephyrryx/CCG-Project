module Util.Advanced where

import Util.Tuple.Apply
import Util.Conditional


fcond :: (x -> (a -> b)) -> (a -> Bool) -> x -> x -> (a -> b)
--    = \h p f g -> cond p (h f) (h g)
--    = \h p -> curry (\(f,g) -> cond p (h f) (h g))
--    = \h p -> curry (\(f,g) -> uncurry (cond p) (h f, h g))
--    = \h p -> curry $ uncurry (cond p) . tmup h
--    = \h -> (curry .) $ (\p -> uncurry (cond p) . tmup h)
--    = (curry.).(\h -> (\x p -> (uncurry (cond p)).x) $ tmup h)
--    = (curry.).(\x p -> (uncurry (cond p)).x).tmup
--    = (curry.).(\x p -> ((.).uncurry.cond) p x).tmup
fcond = (curry.).flip((.).uncurry.cond).tmup

precond :: (a -> b) -> (a -> Bool) -> (b -> c) -> (b -> c) -> (a -> c)
-- precond h = fcond (.h)  = \p f g -> cond p (f.h) (g.h)
precond = fcond . flip (.)

consd :: (a -> Bool) -> b -> b -> (a -> b)
consd = fcond const
