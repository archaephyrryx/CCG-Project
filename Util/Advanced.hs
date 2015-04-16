module Util.Advanced where

import Util.Tuple.Apply
import Util.Conditional

precond :: (a -> Bool) -> (a -> b) -> (b -> c) -> (b -> c) -> (a -> c)
--precond p h f g = cond p (f.h) (g.h)
--precond p h = \f g -> cond p (f.h) (g.h)
--            = curry (\(f,g) -> cond p (f.h) (g.h))
--            = curry (\(f,g) -> uncurry (cond p) (f.h,g.h))
--            = curry $ uncurry (cond p) . tmup (.h)
--precond p = (curry $).((uncurry (cond p)).).tmup.flip (.)
precond = (curry .) . (. (tmup . flip (.))) . (.) . uncurry . cond
