{-# LANGUAGE OverloadedStrings #-}

module RequestParserSpec where

import Control.Exception
import Control.Monad
import Data.Attoparsec
import Data.ByteString
import Network.Panpipes.HTTP.RequestParser
import Network.Panpipes.HTTP.Type
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

    where
      parse_ p input =
          let parser r = case r of
                Partial f  -> parser $ f empty
                Done _ a   -> a
                Fail _ _ a -> error a
          in
           parser $ parse p input
