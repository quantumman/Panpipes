{-# LANGUAGE OverloadedStrings #-}

module RequestParserSpec where

import Control.Monad
import Data.Attoparsec
import Data.ByteString
import Network.Panpipes.HTTP.RequestParser
import Network.Panpipes.HTTP.Type
import Test.Hspec

spec :: Spec
spec =
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

    where
      parse_ p input =
          let parser r = case r of
                Partial f  -> parser $ f empty
                Done _ a   -> a
                Fail _ _ a -> error a
          in
           parser $ parse p input
