module Site where

import Application
import CCG hiding (text)
import Control.Applicative        ((<$>))
import Control.Monad
import Happstack.Server
import App.Core.Base
import Pages.Reform (cardHtml, deckHtml, CFilterSig, DFilterSig)
import qualified Data.ByteString.Char8 as C
import Renderer.Cards
import Renderer.Core
import Renderer.Deck
import Renderer.FilterCard
import Renderer.SingleCard
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

sitename = "HappleJack"

page :: Html -> Page
page = ok . blazeResponse

blazeResponse :: Html -> Response
blazeResponse html = toResponseBS (C.pack "text/html;charset=UTF-8") $ renderHtml html

card :: Renderer Filter
card = base pagename . cardHtml
  where
    pagename = sitename ++ ": Cards"

deck :: Renderer Filter
deck = base pagename . deckHtml
  where
    pagename = sitename ++ ": Deck Builder"
