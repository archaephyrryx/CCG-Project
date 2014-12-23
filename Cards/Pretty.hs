{-# LANGUAGE RecordWildCards #-}

module Cards.Pretty where

import Cards
import Cards.Common
import Cards.Common.Hint
import Cards.Common.Stringe
import Cards.Common.Abbrev
import Cards.Common.Color
import Data.List.Split
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Array.Base

xor :: Bool -> Bool -> Bool
x `xor` y = (x || y) && (not (x && y))

type Width = Int
type Height = Int
type Painting = Card -> Canvas
type Update = Canvas -> Canvas
type CUpdate = Card -> Update
type Canvas = UArray (Int,Int) Char
type Image = [((Int,Int),Char)]

blank :: Height -> Width -> Canvas
blank h w = listArray ((0,0),(h,w)) (replicate ((h+1)*(w+1)) ' ')

render :: Canvas -> [String]
render c = let ((_,_),(_,w)) = bounds c
           in chunksOf (w+1) (map snd (read (dropWhile (/='[') (show c)) :: Image))

prettyShow :: ShowCard
prettyShow x = unlines.render $ (case cardtype x of
        TMane           -> generateMane x
        TFriend         -> generateFriend x
        TEvent          -> generateEvent x
        TResource       -> generateResource x
        TTroublemaker   -> generateTroublemaker x
        TProblem        -> generateProblem x
        )
    where
        h = 60
        w = 50
        generateMane :: Painting
        generateMane c@Mane{..} = let canvas = blank h w
                                  in juxtapose (updateMane c canvas, updateBoosted c canvas) 
        generateFriend :: Painting
        generateFriend c@Friend{..} = let canvas = blank h w
                                      in updateFriend c canvas
        generateEvent :: Painting
        generateEvent c@Event{..} = let canvas = blank h w
                                    in updateEvent c canvas
        generateResource :: Painting
        generateResource c@Resource{..} = let canvas = blank h w
                                          in updateResource c canvas
        generateTroublemaker :: Painting
        generateTroublemaker c@Troublemaker{..} = let canvas = blank h w
                                                  in updateTroublemaker c canvas
        generateProblem :: Painting
        generateProblem c@Problem{..} = let canvas = blank w h
                                        in updateTroublemaker c canvas


{-
 - Mane { name, set, num, rar, keywords, color, power, boosted, text }
 - Friend { name, set, num, rar, keywords, color, cost, req, power, text}
 - Resource { name, set, num, rar, keywords, color, cost, req, power, text }
 - Event { name, set, num, rar, keywords, color, cost, req, power, text }
 - Troublemaker { name, set, num, rar, keywords, power, points, text }
 - Problem { name, set, num, rar, points, keywords, preqs, text }
 -}

updateMane :: CUpdate
updateMane c@Mane{..} canvas = (updateBase c canvas)//start
    where
        (h,w) = snd.bounds$canvas
        (ih,iw) = (30, r-l-8)
        margin = 2
        (t,l,r,b) = (margin, margin, w - margin, h - margin)
        sp = 1
        start = zip [(i,r-1) | i <- [t+3*sp+13..t+3*sp+17]] "START"

updateBoosted :: CUpdate
updateBoosted c@Mane{..} canvas = (updateBase c canvas)//boosted
    where
        (h,w) = snd.bounds$canvas
        (ih,iw) = (30, r-l-8)
        margin = 2
        (t,l,r,b) = (margin, margin, w - margin, h - margin)
        sp = 1
        boosted = zip [(i,r-1) | i <- [t+3*sp+12..t+3*sp+18]] "BOOSTED"

updateBase c canvas = foldl (//) canvas parts
    where
        (h,w) = snd.bounds$canvas
        (ih,iw) = (30, r-l-8)
        margin = 2
        (t,l,r,b) = (margin, margin, w - margin, h - margin)
        sp = 1
        parts = [ topbot, leftright, corners, headline, title, image, legend, textbox, footline ]
        topbot = [((i,j),'-') | i <- [0,h], j <- [0..w]]
        leftright = [((i,j),'|') | j <- [0,w], i <- [0..h]]
        corners = [((i,j),k) | i <- [0,h], j <- [0,w], let k = (if (i==0)`xor`(j==0) then '\\' else '/')]
        headline = (zip [(t+sp,j) | j <- [l..]] (show.cardtype$c)) ++
                   (zip [(t+sp,j) | j <- [r,r-1..0]] (reverse ((show.color$c) ++ "    " ++ (show.val.power$c))))
        title = (zip [(t+2*sp, j) | j <- [l+4..]] (unravel.name$c))
        image = [((i,j),'-') | i <- [t+3*sp,(t+3*sp+30)], j <- [l+4..r-4]] ++
                [((i,j),'|') | j <- [t+3*sp..(t+3*sp+30)], i <- [l+3,r-3]] ++
                prettyPicture (t+3*sp+15,(l+r)`div`2) (ih,iw) (setnum c)
        legend = []
        textbox = []
        footline = []

updateFriend = updateBase
updateResource = updateBase
updateEvent = updateBase
updateTroublemaker = (flip const)
updateProblem = (flip const)

prettyPicture :: (Int,Int) -> (Height,Width) -> String -> Image
prettyPicture (x,y) (h,w) sn = zip [(i,j) | i <- [y-(h`div`2)..y+(h`div`2)], j <- [x-(w`div`2)..x+(w`div`2)]] (repeat ' ')

juxtapose = fst
