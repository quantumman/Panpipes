module Control.Proxy.Network where

import Control.Monad (unless)
import Control.Proxy
import Control.Proxy.Safe
import qualified Data.ByteString as Strict
import Network.Socket hiding (recv)
import Network.Socket.ByteString


type BlockSize = Int


fromTcpS :: Proxy p
         => Socket
         -> BlockSize
         -> ()
         -> Producer (ExceptionP p) Strict.ByteString SafeIO ()
fromTcpS sock blockSize () = do
  bs <- tryIO $ recv sock blockSize
  unless (Strict.null bs) $ respond bs >>= fromTcpS sock blockSize


toTcpD :: Proxy p
       => Socket
       -> ()
       -> Consumer (ExceptionP p) Strict.ByteString SafeIO ()
toTcpD sock () = forever $ do
  bs <- request ()
  tryIO $ sendAll sock bs
