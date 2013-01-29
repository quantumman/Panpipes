{-# LANGUAGE OverloadedStrings #-}

module Network.Panpipes.HTTP.RequestParser (
    method
  , Method(..)
  , version
  ) where

import Control.Applicative
import Data.Attoparsec
import Data.Attoparsec.Char8 (isDigit_w8)
import Data.ByteString
import Data.Char (chr, digitToInt, ord)
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


version :: Parser Version
version = do
  string "HTTP/"
  Version <$> major <* word8' '.' <*> minor
  where
    major =  digitToInt . chr . fromIntegral <$> satisfy isDigit_w8
    minor = major


word8' :: Char -> Parser Word8
word8' = word8 . fromIntegral . ord
