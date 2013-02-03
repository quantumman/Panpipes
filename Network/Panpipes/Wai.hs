module Network.Panpipes.Wai where

import Control.Proxy
import Control.Proxy.Safe
import Data.ByteString
import qualified Network.Panpipes.HTTP.Types as HTTP


type Body p = Producer p ByteString SafeIO ()


data Request p = Request
                 { method  :: HTTP.Method
                 , version :: HTTP.Version
                 , rowUri  :: ByteString
                 , requestHeaders :: [(ByteString, ByteString)]
                 , requestBody    :: Body p
                 }


type ResponseHeaders = [(ByteString, ByteString)]

data Response p = ResponseFile HTTP.Status ResponseHeaders FilePath
                | Response HTTP.Status ResponseHeaders (Body p)
