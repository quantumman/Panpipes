module Network.Panpipes.HTTP.Type where

data Method = Option | Get | Head | Delete | Trace | Connect | Post | Put
            deriving (Show, Eq)