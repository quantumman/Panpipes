{-# LANGUAGE OverloadedStrings #-}

module Network.Panpipes.HTTP.RequestParser (
    method
  , Method(..)
  , version
  , Version(..)
  , headers
  ) where

import Control.Applicative
import Data.Attoparsec
import Data.Attoparsec.Char8 hiding (satisfy)
import qualified Data.ByteString as ByteString (unpack, pack)
import Data.ByteString hiding (notElem, map, concat)
import Data.Char (ord, chr, digitToInt)
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


headers :: Parser [(ByteString, ByteString)]
headers = many $ header <* crlf
    where
      header = (,) <$> fieldName <*> (word8' ':' *> many space *> fieldValue)
      fieldName = ByteString.pack <$> many1 token
      fieldValue = ByteString.pack . concat <$> many (many1 (noneOf "\r\n") <|> lws)
      lws = [32] <$ (crlf *> many1 (word8' ' ' <|> word8' '\t')) -- 32 == ' '

token :: Parser Word8
token = satisfy isToken
    where
      isToken w = w `notElem` [0..31] && w /= 127 && w `notElem` separators
      separators = map (fromIntegral . ord) "()<>@,;:\\\"/[]?={} \t"


crlf = string "\r\n"

noneOf :: ByteString -> Parser Word8
noneOf bs = satisfy (`notElem` ByteString.unpack bs)

word8' :: Char -> Parser Word8
word8' = word8 . fromIntegral . ord
