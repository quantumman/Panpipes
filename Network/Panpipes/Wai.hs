module Network.Panpipes.Wai where

import Control.Proxy
import Control.Proxy.Safe
import Data.ByteString


type Body p = Producer p ByteString SafeIO ()
