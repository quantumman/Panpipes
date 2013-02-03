module Control.Proxy.Attoparsec where

import qualified Data.ByteString as Strict


data ParsingResult r
  = Success r Strict.ByteString
  | Error
    { context :: [String]
    , message :: String
    }
