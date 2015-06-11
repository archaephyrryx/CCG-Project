module App.SingleCard where
------------------------------
import Control.Applicative
import Control.Monad
------------------------------
import Data.List
------------------------------
import App.Core
import Renderer.Core
import Renderer.SingleCard
import API.Database
import CCG

appname = "HappleJack"

single :: Renderer String
single s = let c = fromJust.getOne $ cardDB @= (ravel s :: SetNum)
           in base appname $ ([content (renderCard c), sidebar (cardInfo c)])
