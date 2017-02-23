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
    it "parses a full URI" $
        "postgres://a:b@c:123/d" `shouldParseTo` "user=a password=b host=c port=123 dbname=d"

    it "parses a URI without password" $
        "postgres://a@c:123/d" `shouldParseTo` "user=a host=c port=123 dbname=d"

    it "parses a URI without credentials" $
        "postgres://c:123/d" `shouldParseTo` "host=c port=123 dbname=d"

    it "parses a URI without port" $
        "postgres://c/d" `shouldParseTo` "host=c dbname=d"

    it "parses a URI without anything" $
        "postgres:///" `shouldParseTo` ""

    it "rejects invalid URIs" $
        "|7^bvk3" `shouldRejectWith` "Invalid URI: |7^bvk3"

    -- N.B. Can't figure out a way to make *just* the Authority invalid
    -- it "rejects invalid Authorities" $ do

    it "rejects unexpected schemes" $
        "https://example.com" `shouldRejectWith` "Invalid scheme: https://, expecting postgres://"

shouldParseTo :: String -> String -> Expectation
x `shouldParseTo` y = parsePGConnectionString x `shouldBe` Right y

shouldRejectWith :: String -> String -> Expectation
x `shouldRejectWith` y = do
    let x' = parsePGConnectionString x :: Either String String

    x' `shouldBe` Left y
