module Network.Panpipes.HTTP.Types where

import Data.ByteString

data Method = Option | Get | Head | Delete | Trace | Connect | Post | Put
            deriving (Show, Eq)


data Version = Version { major :: Int , minor :: Int }
               deriving (Show, Eq)


data PartialRequest = PartialRequest
    { method  :: Method
    , uri     :: ByteString
    , version :: Version
    , headers :: [(ByteString, ByteString)]
    } deriving (Show, Eq)


data Status = Status
    { code :: Int
    , reasonPhrase :: String
    } deriving (Show, Eq)
