{-# LANGUAGE RecursiveDo #-}
module Reactive.Util where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Monad.Fix
{-
import qualified Reactive.Banana.Internal.Combinators as Prim
import Reactive.Banana.Types
-}

anyEvent :: [Event ()] -> Event ()
anyEvent = foldl1 (unionWith (\_ _ -> ()))

priorityUnion :: [Event a] -> Event a
priorityUnion = foldl1 (unionWith const)

aggregate :: [b] -> (a -> IO b) -> Event a -> MomentIO (Behavior [b])
aggregate bs new eAdd = mdo
    eAddition <- mapEventIO new eAdd
    let eAll = flip (:) <$> bAll <@> eAddition
    bAll <- stepper bs eAll
    return bAll

restepper :: b -> (b -> a -> b) -> Event a -> MomentIO (Behavior b)
restepper z f e = mdo
    let eDelta = f <$> bDelta <@> e
    bDelta <- stepper z eDelta
    return bDelta

override :: Behavior Bool -> Behavior a -> Event a -> Event a
override on feed source = priorityUnion [ feed <@ whenE on source
                                        , source
                                        ]
