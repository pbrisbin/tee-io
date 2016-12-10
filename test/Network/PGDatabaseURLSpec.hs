module Network.PGDatabaseURLSpec
    ( main
    , spec
    ) where

import Prelude

import Network.PGDatabaseURL

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "PGDatabaseURLSpec" $ do
    it "parses a full URI" $ do
        "postgres://a:b@c:123/d" `shouldParseTo` "user=a password=b host=c port=123 dbname=d"

    it "parses a URI without password" $ do
        "postgres://a@c:123/d" `shouldParseTo` "user=a host=c port=123 dbname=d"

    it "parses a URI without credentials" $ do
        "postgres://c:123/d" `shouldParseTo` "host=c port=123 dbname=d"

    it "parses a URI without port" $ do
        "postgres://c/d" `shouldParseTo` "host=c dbname=d"

    it "parses a URI without anything" $ do
        "postgres:///" `shouldParseTo` ""

    it "rejects invalid URIs" $ do
        "|7^bvk3" `shouldRejectWith` "Invalid URI: |7^bvk3"

    -- TODO: can't figure out a way to make *just* the Authority invalid
    -- it "rejects invalid Authorities" $ do

    it "rejects unexpected schemes" $ do
        "https://example.com" `shouldRejectWith` "Invalid scheme: https://, expecting postgres://"

shouldParseTo :: String -> String -> Expectation
x `shouldParseTo` y = do
    parsePGConnectionString x `shouldBe` Right y

shouldRejectWith :: String -> String -> Expectation
x `shouldRejectWith` y = do
    let x' = parsePGConnectionString x :: Either String String

    x' `shouldBe` Left y
