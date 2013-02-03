module Control.Proxy.Attoparsec where

import Control.Proxy
import Data.Attoparsec.ByteString hiding (Parser(..))
import Data.Attoparsec.Types
import qualified Data.ByteString as Strict
import Data.ByteString.Char8 (pack)


data ParsingResult r
  = Success r Strict.ByteString
  | Error
    { context :: [String]
    , message :: String
    }


parseD :: (Monad m, Proxy p)
       => Parser Strict.ByteString r
       -> ()
       -> Pipe p Strict.ByteString (ParsingResult r) m ()
parseD parser _ = runIdentityP $ go (parse parser)
  where
    go k0 = do
      xs <- request ()
      case k0 xs of
        Partial k -> go k
        Fail _ context message -> respond $ Error context message
        Done rest r -> respond $ Success r rest
