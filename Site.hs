{-# LANGUAGE FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, ScopedTypeVariables,
	TypeFamilies, TypeSynonymInstances,
	OverloadedStrings #-}

module Site where

import Cards.Common hiding (Text)
import Control.Applicative        ((<$>))
import Control.Monad
import Data.Char                  (toLower)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Happstack.Server
import Pages.Card
import Pages.Common
import Pages.Reform
import Data.Text.Lazy (Text)
import qualified Pages.Reform as Reformed
import qualified Pages.Vanilla as Vanilla
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString.Char8 as C
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 (Html)
import Text.Blaze
import Text.Reform
import Text.Reform.Happstack
import Text.Reform.Blaze.String

sitename = "HappleJack"

page :: Html -> ServerPartT IO Response
page = ok . blazeResponse

home :: Html
home = base pagename (H.div $ do
                        H.h1 $ (H.toHtml $ pagename)
                        H.p $ (H.toHtml $ "Welcome to " <> pagename <> ", an in-development website for the MLP:CCG.")
                        H.p $ do "This site was inspired by, and modelled on, "
                                 H.a ! A.href "http://ponyhead.com" $ "PonyHead"
                        H.p $ "This site has several components that behave similarly to those of PonyHead, as well as several novel features. Bear in mind, though, that this site is still under development, so those features might be missing, or buggy if they are present."
                        )
    where
        pagename = sitename

cardHtml :: Html -> Html
cardHtml = base (sitename ++ ": Cards")

deckHtml :: Html -> Html
deckHtml = base (sitename ++ ": Deck Builder")

blazeResponse :: Html -> Response
blazeResponse html = toResponseBS (C.pack "text/html;charset=UTF-8") $ renderHtml html

card :: CFilterSig (ServerPart Response)
card x mc ms mr mt = formHandler "/card" "card" (cardForm x mc ms mr mt)
    
deck :: DFilterSig (ServerPart Response)
deck mc ms mr mt = formHandler "/deck" "deck" (deckForm mc ms mr mt)

formHandler :: Text -> Text -> SimpleForm Filter -> ServerPart Response
formHandler act str theForm = do
    result <- happstackEitherForm (form act) str theForm
    case result of
        (Left view) -> page $ cardHtml view
        (Right a) -> page . cardHtml $ renderFilter a
