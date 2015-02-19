{-# LANGUAGE RecordWildCards #-}

module Cards.Pretty where

import Cards
import Cards.Common
import Data.List.Split
import Data.List
import Data.Maybe (fromJust)
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
data Layout = Layout { w :: Width
                     , h :: Height
                     , iw :: Width
                     , ih :: Height
                     , vm :: Int
                     , hm :: Int
                     , sp :: Int
                    }

defaultLayout :: Layout
defaultLayout = let w = 52
                    h = 45
                    iw = 42
                    ih = 15
                    vm = 1
                    hm = 2
                    sp = 1
                in Layout{..}


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
        layout@Layout{..} = defaultLayout
        generateMane :: Painting
        generateMane c@Mane{..} = let canvas = blank h w
                                  in juxtapose (updateMane c canvas layout, updateBoosted c canvas layout) 
        generateFriend :: Painting
        generateFriend c@Friend{..} = let canvas = blank h w
                                      in updateFriend c canvas layout
        generateEvent :: Painting
        generateEvent c@Event{..} = let canvas = blank h w
                                    in updateEvent c canvas layout
        generateResource :: Painting
        generateResource c@Resource{..} = let canvas = blank h w
                                          in updateResource c canvas layout
        generateTroublemaker :: Painting
        generateTroublemaker c@Troublemaker{..} = let canvas = blank h w
                                                  in updateTroublemaker c canvas layout
        generateProblem :: Painting
        generateProblem c@Problem{..} = let canvas = blank w h
                                        in updateTroublemaker c canvas layout


{-
 - Mane { name, set, num, rar, keywords, color, power, boosted, text }
 - Friend { name, set, num, rar, keywords, color, cost, req, power, text}
 - Resource { name, set, num, rar, keywords, color, cost, req, power, text }
 - Event { name, set, num, rar, keywords, color, cost, req, power, text }
 - Troublemaker { name, set, num, rar, keywords, power, points, text }
 - Problem { name, set, num, rar, points, keywords, preqs, text }
 -}

updateMane c@Mane{..} canvas lo@Layout{..}= foldl (//) (updateBase c canvas lo) [side,textbox]
    where
        (t,l,r,b) = (vm, hm, w - hm, h - vm)
        str = "START"
        up = (t+3*sp)+(ih`div`2)-(length str `div` 2)
        dn = (t+3*sp)+(ih`div`2)+(length str `div` 2)
        side = zip [(i,r-1) | i <- [up..dn]] str
        (it, il, ir, ib) = (t+5*sp, (w`div`2)-(iw`div`2), (w`div`2)+(iw`div`2), t+5*sp+ih)
        textbox = concat [ zip [(ib+5*sp+v,j) | j <- [l+sp..]] (tline!!v) | v <- [0..(length tline - 1)]]
        tline = concatMap (chunksOf (r-l-2*sp)) (splitOn " <P> " (fromJust (stripPrefix "Front: " (head (splitOn " Back: " (unravel text))))))


updateBoosted c@Mane{..} canvas lo@Layout{..}= foldl (//) (updateBase c canvas lo) [headline, side, textbox]
    where
        (t,l,r,b) = (vm, hm, w - hm, h - vm)
        str = "BOOSTED"
        up = (t+3*sp)+(ih`div`2)-(length str `div` 2)
        dn = (t+3*sp)+(ih`div`2)+(length str `div` 2)
        headline = (zip [(t+sp,j) | j <- [l..]] (show.cardtype$c)) ++
                   (zip [(t+sp,j) | j <- [r,r-1..0]] (reverse ((show color) ++ "    " ++ (show.val$boosted))))
        side = zip [(i,r-1) | i <- [up..dn]] str
        (it, il, ir, ib) = (t+5*sp, (w`div`2)-(iw`div`2), (w`div`2)+(iw`div`2), t+5*sp+ih)
        textbox = concat [ zip [(ib+5*sp+v,j) | j <- [l+sp..]] (tline!!v) | v <- [0..(length tline - 1)]]
        tline = concatMap (chunksOf (r-l-2*sp)) (splitOn " <P> " (last (splitOn " Back: " (unravel text))))

updateBase c canvas lo@Layout{..}= foldl (//) canvas parts
    where
        (t,l,r,b) = (vm, hm, w - hm, h - vm)
        parts = [ topbot, leftright, corners, headline, title, image, legend, footline ]
        topbot = [((i,j),'-') | i <- [0,h], j <- [0..w]]
        leftright = [((i,j),'|') | j <- [0,w], i <- [0..h]]
        corners = [((i,j),k) | i <- [0,h], j <- [0,w], let k = (if (i==0)`xor`(j==0) then '\\' else '/')]
        headline = (zip [(t+sp,j) | j <- [l..]] (show.cardtype$c)) ++
                   (zip [(t+sp,j) | j <- [r,r-1..0]] (reverse ((show.color$c) ++ "    " ++ (show.val.power$c))))
        title = (zip [(t+2*sp, j) | j <- [l..]] (unravel.name$c))
        (it, il, ir, ib) = (t+5*sp, (w`div`2)-(iw`div`2), (w`div`2)+(iw`div`2), t+5*sp+ih)
        image = [((i,j),'-') | i <- [it-1,ib+1], j <- [il..ir]] ++
                [((i,j),'|') | i <- [it-1..ib+1], j <- [il-1,ir+1]] ++
                prettyPicture ((it + ib)`div`2, (il + ir)`div`2) (ih, iw) (setnum c)
        legend = (zip [(ib+3*sp, j) | j <- [l..]] (unwords.(map unravel).keywords$c))
        footline = zip [(b-sp,j) | j <- [r,r-1..0]] (reverse ((brief.rar$c) ++ "     " ++ (brief.set$c) ++ " " ++ (show.num$c)))

updateCard c canvas lo@Layout{..} = (updateBase c canvas lo)//textbox 
    where
        (t,l,r,b) = (vm, hm, w - hm, h - vm)
        (it, il, ir, ib) = (t+5*sp, (w`div`2)-(iw`div`2), (w`div`2)+(iw`div`2), t+5*sp+ih)
        textbox = concat [ zip [(ib+5*sp+v,j) | j <- [l+sp..]] (tline!!v) | v <- [0..(length tline - 1)]]
        tline = concatMap (chunksOf (r-l-2*sp)) (splitOn " <P> " (unravel.text$c))

updateFriend = updateCard
updateResource = updateCard
updateEvent = updateCard

updateTroublemaker c canvas lo@Layout{..}= foldl (//) canvas parts
    where
        (t,l,r,b) = (vm, hm, w - hm, h - vm)
        parts = [ topbot, leftright, corners, headline, title, image, legend, textbox, footline ]
        topbot = [((i,j),'-') | i <- [0,h], j <- [0..w]]
        leftright = [((i,j),'|') | j <- [0,w], i <- [0..h]]
        corners = [((i,j),k) | i <- [0,h], j <- [0,w], let k = (if (i==0)`xor`(j==0) then '\\' else '/')]
        headline = (zip [(t+sp,j) | j <- [l..]] (show.cardtype$c)) ++
                   (zip [(t+sp,j) | j <- [r,r-1..0]] (reverse ((show.val.power$c))))
        title = (zip [(t+2*sp, j) | j <- [l+4..]] (unravel.name$c))
        (it, il, ir, ib) = (t+5*sp, (w`div`2)-(iw`div`2), (w`div`2)+(iw`div`2), t+5*sp+ih)
        image = [((i,j),'-') | i <- [it-1,ib+1], j <- [il..ir]] ++
                [((i,j),'|') | i <- [it-1..ib+1], j <- [il-1,ir+1]] ++
                prettyPicture ((it + ib)`div`2, (il + ir)`div`2) (ih, iw) (setnum c)
        legend = (zip [(t+sp,j) | j <- [r,r-1..0]] (reverse ("Bonus: "++(show.val.points$c))))
        textbox = concat [ zip [(ib+5*sp+v,j) | j <- [l+sp..]] (tline!!v) | v <- [0..(length tline - 1)]]
        tline = concatMap (chunksOf (r-l-2*sp)) (splitOn " <P> " (unravel.text$c))
        footline = zip [(b-sp,j) | j <- [r,r-1..0]] (reverse ((brief.rar$c) ++ "     " ++ (brief.set$c) ++ " " ++ (show.num$c)))

updateProblem = (flip const)

prettyPicture :: (Int,Int) -> (Height,Width) -> String -> Image
prettyPicture (y,x) (h,w) sn = zip [(i,j) | i <- [y-((h-1)`div`2)..y+((h-1)`div`2)], j <- [x-((w-1)`div`2)..x+((w-1)`div`2)]] (repeat ' ')

juxtapose (a,b) = let h = fst.snd.bounds$a
                      w = snd.snd.bounds$a
                    in (blank h (2*w+1))//(assocs a)//(map (\((x,y),z) -> ((x,w+1+y),z)) (assocs b))
