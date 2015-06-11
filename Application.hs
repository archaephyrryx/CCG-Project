module Application where

import Happstack.Server

type Page = ServerPartT IO Response
