module App where

-----------------------
import Widgets.Table
import Widgets.MultiSelect
import Widgets.MonoSelect
import Widgets.Links
import Widgets.Radio
import Widgets.Core
-----------------------
import Data.List
-----------------------
import App.Mode


app :: IO ()
app = do
  w <- frame [ text := "FRPony" ]
  nav <- table w []
  content <- table w []
  let c = _tab content
  ---- Home
  home <- staticText c []
  ---- FilterCard





  let networkDescription :: MomentIO ()
      networkDescription = mdo

      satnav <- radio nav modes bMode show
      bMode <- stepper Home $ portents satnav

      ---- HOME MODE -----
      house <- frozenText home homeText

      let groupHome =
            Group { _members = [Item house]
                  , _layout = widget . only
                  }

          aspectHome = aspect groupHome Home Nothing

      ---





      liftIO $ set f [layout := margin 10 $ column 5 $ [widget satnav, widget content]]

  network <- compile networkDescription
  actuate network
