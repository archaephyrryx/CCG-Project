module Renderer.Deck where
-------------------------------------------------
import Prelude hiding (div, span)
import Data.Maybe
import Data.List.Split
import Data.List hiding (span)
--------------------------------------------------
import API.Database
import Data.IxSet
--------------------------------------------------
import CCG
import Util
--------------------------------------------------
import Renderer.Core
import Renderer.Cards
import Renderer.SingleCard
--------------------------------------------------

construct :: Deck -> Rendered'
construct d = [ structure "mane" (mheader nmane) manes
              , structure "problem" (pheader nprob start) probs
              , structure "draw" (dheader ndraw) draws
              ]
    where
      parts@(manes, probs, draws) = tpart d
      lens@(nmane,nprob,ndraw) = mhall (length,length,length) parts
      start = hasStarting probs

structure :: String -> Rendered -> DeckP -> Rendered
structure pre hed dek = div #. pre #+ [ div #. (pre++"-deck") #: (h2 #. (pre++"-title") #: hed)
                                      , hr
                                      , div #. (pre++"-cards") #: (con.struct $ dek)
                                      ]

mheader :: Int -> Rendered
mheader n = span #$ (string $ "Manes ("++(show n)++")")

pheader :: Int -> Bool -> Rendered
pheader n s = span #+ ((s?:nonstarter) [span #$ (string $ "Problem Deck ("++(show n)++"/10)")])
  where
    nonstarter :: [Rendered] -> [Rendered]
    nonstarter = (++[span #. "no-start" #$ string "No Starting Problem!"])

dheader :: Int -> Rendered
dheader n = span #$ string ("Draw Deck ("++(show n)++"/45)")

con :: UniCard c => [(c,Int)] -> Rendered
con xs = div #. "card-box" #+ (map ((div #. "card-line" #:).cline) xs)
  where
    cline (c,n) = span #. "cline" #+ (account [ span #. "ctab" #: ctab c, span #. "cname" #$ (string $ uname c)])
      where
        account = ((++[span #. "ccount badge" #$ (string $ show n)])?+n)
        ctab = cond (utype.=TProblem) conf empower
