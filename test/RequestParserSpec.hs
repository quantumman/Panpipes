{-# LANGUAGE OverloadedStrings #-}

module RequestParserSpec where

import Control.Exception
import Control.Monad
import Data.Attoparsec
import qualified Data.ByteString as ByteString
import Network.Panpipes.HTTP.RequestParser
import Network.Panpipes.HTTP.Types (Method)
import qualified Network.Panpipes.HTTP.Types as Types (PartialRequest(..))
import Network.Panpipes.HTTP.Types hiding (PartialRequest(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "method" $
    it "parses http method" $
      forM_ [ ("OPTION" , Option)
            , ("GET"    , Get)
            , ("HEAD"   , Head)
            , ("DELETE" , Delete)
            , ("TRACE"  , Trace)
            , ("CONNECT", Connect)
            , ("POST"   , Post)
            , ("PUT"    , Put)
            ]
      $ \(input, expected) -> parse_ method input `shouldBe` expected

  describe "version" $ do
    it "parses http version" $ do
      parse_ version "HTTP/1.0" `shouldBe` Version { major = 1, minor = 0 }
      parse_ version "HTTP/1.1" `shouldBe` Version { major = 1, minor = 1 }
      parse_ version "HTTP/2.2" `shouldBe` Version { major = 2, minor = 2 }
    it "cannot parse illegal input" $
      evaluate(parse_ version "1.0") `shouldThrow` anyErrorCall

  describe "headers" $ do
    it "parses a http headers" $
      parse_ headers "K1: V1\r\n" `shouldBe` [("K1", "V1")]
    it "parses http headers" $ do
      let input = "K1:V1\r\nK2: V2\r\n"
      parse_ headers input `shouldBe` [("K1", "V1"), ("K2", "V2")]
    it "accepts multi-line value" $ do
      let input1 = "K1:V1\r\nK2: V2\r\n   -A\r\n"
      parse_ headers input1 `shouldBe` [("K1", "V1"), ("K2", "V2 -A")]
      let input2 = "K1:V1\r\nK2: V2\r\n  \t -B\r\n"
      parse_ headers input2 `shouldBe` [("K1", "V1"), ("K2", "V2 -B")]
    it "does not accept the header having space(s) before `:`" $ do
      parse_ headers "K1: V1\r\nK2 : V2\r\n" `shouldBe` [("K1", "V1")]
      parse_ headers "K1 : V1\r\nK2: V2\r\n" `shouldBe` []
      parse_ headers "K1  : V1\r\n" `shouldBe` []

  describe "request" $
    it "parses http request excepting for body" $ do
      let req = ByteString.concat [ "POST /enlighten/calais.asmx HTTP/1.1\r\n"
                                  , "Host: localhost\r\n"
                                  , "Content-Type: text/xml;\r\n"
                                  , "              charset=utf-8\r\n"
                                  , "Content-Length: length\r\n"
                                  , "\r\n"
                                  , "BODY"
                                  ]
      parse_ request req
        `shouldBe` Types.PartialRequest
                   { Types.method  = Post
                   , Types.uri     = "/enlighten/calais.asmx"
                   , Types.version = Version 1 1
                   , Types.headers = [ ("Host", "localhost")
                                     , ("Content-Type", "text/xml; charset=utf-8")
                                     , ("Content-Length", "length")
                                     ]
                   }
    where
      parse_ p input =
          let parser r = case r of
                Partial f  -> parser $ f ByteString.empty
                Done _ a   -> a
                Fail _ _ a -> error a
          in
           parser $ parse p input
