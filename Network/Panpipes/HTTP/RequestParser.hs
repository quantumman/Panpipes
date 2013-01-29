{-# LANGUAGE OverloadedStrings #-}

module Network.Panpipes.HTTP.RequestParser (
    method
  , Method(..)
  ) where

import Control.Applicative
import Data.Attoparsec
import Data.ByteString
import Data.Char (ord)
import Data.Word (Word8(..))
import Network.Panpipes.HTTP.Type


method :: Parser Method
method = Option <$ string "OPTION"
         <|> Get <$ string "GET"
         <|> Head <$ string "HEAD"
         <|> Delete <$ string "DELETE"
         <|> Trace <$ string "TRACE"
         <|> Connect <$ string "CONNECT"
         <|> word8' 'P' *> (Post <$ string "OST" <|> Put <$ string "UT")


word8' :: Char -> Parser Word8
word8' = word8 . fromIntegral . ord
