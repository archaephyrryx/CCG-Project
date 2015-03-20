{-# LANGUAGE DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, OverlappingInstances,
    IncoherentInstances,
    OverloadedStrings, QuasiQuotes, RecordWildCards,
    ScopedTypeVariables, TemplateHaskell, TypeFamilies,
    TypeSynonymInstances #-} 

module Pages.Card where
-------------------------------------------------
import Control.Applicative
import Control.Applicative.Indexed  ( IndexedFunctor(..) , IndexedApplicative(..))
import Control.Monad
import Control.Monad.Identity       ( Identity(runIdentity) )
import Data.Char
import Data.Data		            ( Data, Typeable )
import Data.Maybe
import Data.String                  ( IsString(fromString) ) 
import Data.List.Split
import Data.List
import qualified Data.Text          as Strict
import qualified Data.Text.Lazy     as Lazy
import Data.Text.Lazy               ( Text )
import Happstack.Server
import Happstack.Server.HSP.HTML
import Happstack.Server.XMLGenT
import HSP
import HSP.Monad                    ( HSPT(..) )
import Language.Haskell.HSX.QQ      ( hsx )
import Text.Reform.Happstack
import Text.Reform.HSP.Text

import Text.Reform
    ( CommonFormError(..), Proof(..), (++>), (<++), commonFormErrorStr
    , Form, FormError(..), decimal, prove, transformEither, transform )
--------------------------------------------------
import API.Database
import Data.IxSet
import Data.Map                   ( Map )
import qualified Data.Map         as Map
import Data.Set                   ( Set )
import qualified Data.Set         as Set
import Control.Exception	        ( bracket )
import Control.Monad.Reader         ( ask )
import Control.Monad.State	        ( get, put )
--------------------------------------------------
import CCG hiding (Text)
--------------------------------------------------
import Application
import Reformation
import Pages.Common (template, base)
---------------------------------------------------

type SetNum = (CSet, Number)
type GCR = GenCard -> Html

instance Read SetNum where
    readsPrec = const readsSN

readsSN :: ReadS SetNum
readsSN x = let (s, n) = splitAt 2 x in [((readCS s, readN n),"")]

single :: String -> Html
single x = let (s,n) = read x :: SetNum
           in renderCard (head.toList $ (cardDB @= (s) @= (n)))

renderCard :: GCR
renderCard g@GenCard{..} = base (unravel name) $
  [hsx|
    <div>
      <div class="card-imgs">
        <% cardImgs g %>
      </div>
      <div class="card-text">
        <% cardText g %>
      </div>
      <div class="card-info">
        <% cardInfo g %>
      </div>
    </div> :: Html
  |]

cardImgs :: GCR
cardImgs g@GenCard{..} = let urls = curls g in
  [hsx|
    <table>
      <% mapM cimage urls %>
    </table> :: Html
  |]

curls :: GenCard -> [String]
curls g@GenCard{..} = let sn = genset g in map (("/res/cards/"++sn)++) (suffixes ctype)
    where
      suffixes :: CardType -> [String]
      suffixes TMane = [".jpg","b.jpg"]
      suffixes _ = [".jpg"]

cimage :: String -> Html
cimage s =
  [hsx|
    <tr>
      <td>
        <a [ "href" := s :: Attr Text String ]><img class="card" [ "src" := s :: Attr Text String ]/></a>
      </td>
    </tr> :: Html
  |]

cardText :: GCR
cardText g@GenCard{ctype=TMane, ..} =
  [hsx|
    <dl>
      <div>
        <dt>Card Text</dt>
        <dd><% front %></dd>
      </div>
      <div>
        <dt>Boosted Card Text</dt>
        <dd><% back %></dd>
      </div>
    </dl> :: Html
  |]
  where
    front = brunlines (splitOn "<P>" (fromJust (stripPrefix "Front: " (head (splitOn " Back: " (unravel text))))))
    back = brunlines (splitOn "<P>" (last (splitOn " Back: " (unravel text))))
cardText g@GenCard{..} =
  [hsx|
    <dl>
      <div>
        <dt>Card Text</dt>
        <dd><% cardtext %></dd>
      </div>
    </dl> :: Html
  |]
  where
    cardtext = brunlines (splitOn "<P>" (unravel text))

brunlines :: [String] -> GCL
brunlines xs =
  [hsx|
    <%>
      <% mapM brline xs %>
    </%>
  |]
  where
      brline :: String -> Html
      brline s = [hsx| <p> <% s %> </p> :: Html |]

cardInfo :: GCR
cardInfo g@GenCard{..} =
  [hsx|
    <div>
      <div class="panel panel-default quick-facts">
        <div class="panel-heading"><h3 class="panel-title">Quick Facts</h3></div>
        <div class="panel-body">
          <ul>
            <% info g %>
          </ul>
        </div>
      </div>
    </div> :: Html
  |]

info :: GenCard -> GCL
info g@GenCard{..} = let items = (maskfilter mask allitems) in
  [hsx|
    <%>
      <% mapM renderi items %>
    </%>
  |]
  where
    mask :: [Bool]
    mask = typemask ctype
    renderi :: (String, GCR) -> Html
    renderi (nam, gcr) = [hsx| <li> <b><% nam %>:</b> <% gcr g %> </li> :: Html |]
    maskfilter [] [] = []
    maskfilter (x:xs) (y:ys) = let ts = maskfilter xs ys in if x then y:ts else ts

allitems :: [(String, GCR)]
allitems = [ ("Type", typeLink)
           , ("Rarity", rarLink)
           , ("Color", colLink)
           , ("Power", empower)
           , ("Cost", appraise)
           , ("Req", reqtify)
           , ("Confront", conf)
           , ("Opponent Confront", conf')
           , ("Traits", cardTraits)
           ]

typemask :: CardType -> [Bool]
typemask x = map (==1) $ case x of
    TMane         -> [1,1,1,1,0,0,0,0,1]
    TFriend       -> [1,1,1,1,1,1,0,0,1]
    TEvent        -> [1,1,1,1,1,1,0,0,1]
    TResource     -> [1,1,1,1,1,1,0,0,1]
    TTroublemaker -> [1,1,0,1,0,0,0,0,1]
    TProblem      -> [1,1,0,0,0,0,1,1,0]

propLink :: String -> (GenCard -> String) -> (GCR)
propLink s f g =
  [hsx|
    <a [ "href" := ("/card?"++s++"="++(f g)) :: Attr Text String ]>
      <% f g %>
    </a> :: Html
  |]

setLink :: GCR
setLink = propLink "set" (show.gset)
typeLink :: GCR
typeLink = propLink "type" (show.ctype)
rarLink :: GCR
rarLink = propLink "rarity" (show.grar)
colLink :: GCR
colLink = propLink "color" (maybe "Wild" show . mcolor)


reqtify :: GCR
reqtify g@GenCard{..} = cbox (show.val<$>mreq, mcolor)
empower :: GCR
empower g@GenCard{..} = cbox (show.val<$>mpower, mcolor)
appraise :: GCR
appraise g@GenCard{..} = cbox (show.val<$>mcost, Nothing)
conf :: GCR
conf g@GenCard{ctype=TProblem, ..} =
  [hsx|
    <span>
      <% mapM (\(y,x) -> cbox (pure.show.val $ x, pure $ y)).fst.fromJust $ mpreqs %>
    </span> :: Html
  |]
conf _ = cbox (Nothing, Nothing)
conf' :: GCR
conf' g@GenCard{ctype=TProblem, ..} = (\x -> cbox (Just (show.val $ x), Nothing)).snd.fromJust $ mpreqs
conf' _ = cbox (Nothing, Nothing)

cardTraits :: GCR
cardTraits g@GenCard{..} =
  [hsx|
    <span>
      <% mapM keyToTrait keywords %>
    </span> :: Html
  |]

keyToTrait :: Keyword -> Html
keyToTrait k =
  [hsx|
    <span>
      <% unbrace (unravel k) %>
    </span> :: Html
  |]

unbrace :: String -> String
unbrace [] = []
unbrace x | head x == '[' && last x == ']' = init.tail $ x
          | otherwise = x

cbox :: (Maybe String, Maybe Color) -> Html
cbox (Nothing,_) = [hsx|<span/>|]
cbox (Just s, c) = 
  [hsx|
    <span [ "class" := unwords ["element","label",(colorize c)] :: Attr Text String]>
      <% s %>
    </span> :: Html
  |]
  where
    colorize :: Maybe Color -> String
    colorize (Nothing) = "NoColor"
    colorize (Just Wild) = "Wild"
    colorize (Just c) = show c
    
pronounce :: (String, String) -> Html
pronounce (nam, code) =
  [hsx|
    <a [ "href" := ("card/"++code) :: Attr Text String ]>
      <% nam %>
    </a> :: Html
  |]
