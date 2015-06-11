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
           in base appname $ (


Foot :: Rendered
Foot = span #. "footer" #+ [ devel , code , proj ]
    where
        base = "https://github.com/archaephyrryx"
        devel = p #. "dev" #+ [string "Developer:", glue, gitarch]
            where gitarch = hlink base "Archaephyrryx"
        code =  p #. "cod" #+ [string "Project code on", glue, gitpage]
            where gitpage = hlink (base++"/CCG-Project/tree/Happstack") "GitHub"
        proj = p #. "proj" #+ [string "Sister projects also on", glue, githome ]
            where githome = hlink (base++"/CCG-Project") "GitHub"
