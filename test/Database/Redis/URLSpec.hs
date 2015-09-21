module Database.Redis.URLSpec
    ( spec
    , main
    ) where

import Prelude
import Test.Hspec
import Database.Redis (ConnectInfo(..), PortID(..))
import Database.Redis.URI (parseURI)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "parseURI" $ do
    it "parses to the correct ConnectInfo" $ do
        let uri = "redis://user:pass@redis.host.com:1234"
            Just ConnInfo{..} = parseURI uri

        connectHost `shouldBe` "redis.host.com"
        connectPort `shouldBe` PortNumber 1234
        connectAuth `shouldBe` Just "pass"
