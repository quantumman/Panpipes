{-# LANGUAGE RankNTypes #-}

module Network.Panpipes.Wai where

import Control.Proxy
import Control.Proxy.Safe
import Data.ByteString
import qualified Network.Panpipes.HTTP.Types as HTTP


type Body = forall p. Proxy p => Producer (ExceptionP p) ByteString SafeIO ()


data Request = Request
               { method  :: HTTP.Method
               , version :: HTTP.Version
               , rowUri  :: ByteString
               , requestHeaders :: [(ByteString, ByteString)]
               , requestBody    :: Body
               }


type ResponseHeaders = [(ByteString, ByteString)]

data Response = ResponseFile HTTP.Status ResponseHeaders FilePath
              | Response HTTP.Status ResponseHeaders Body


data Application = Application (Request -> IO Response)
