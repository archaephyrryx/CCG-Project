module Application where

import Happstack.Server
import Happstack.Server.HSP.HTML
import Happstack.Server.XMLGenT
import HSP
import HSP.Monad (HSPT(..))

type Page = ServerPartT IO Response
type App m  = XMLGenT (App' m)
type App' m = HSPT XML (ServerPartT m)
type Html = App IO XML
type GCL = GenChildList (App' IO)
type GAL = GenAttributeList (App' IO)
