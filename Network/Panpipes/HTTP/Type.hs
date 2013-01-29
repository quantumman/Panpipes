module Network.Panpipes.HTTP.Type where

data Method = Option | Get | Head | Delete | Trace | Connect | Post | Put
            deriving (Show, Eq)


data Version = Version { major :: Int , minor :: Int }
               deriving (Show, Eq)
