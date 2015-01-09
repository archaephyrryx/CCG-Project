module Application where

import Happstack.Server
import Happstack.Server.HSP.HTML
import Happstack.Server.XMLGenT
import HSP
import HSP.Monad (HSPT(..))

type Pager = ServerPartT IO Response
type Paginator = ServerPartT IO XML
type AppT m  = XMLGenT (AppT' m)
type AppT' m = HSPT XML (ServerPartT m)
type Html = AppT IO XML
type Html' = AppT' IO XML
type GCL = GenChildList (AppT' IO)
