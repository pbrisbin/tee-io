module Network.S3URLSpec
    ( main
    , spec
    ) where

import Prelude

import Control.Lens
import Data.Aeson
import Data.Monoid ((<>))
import Network.S3URL
import Network.AWS
    ( Endpoint
    , Region(..)
    , _svcEndpoint
    , endpointHost
    , endpointPort
    , endpointSecure
    )
import Network.AWS.S3 (BucketName(..))
import Test.Hspec

import qualified Data.ByteString.Lazy.Char8 as C8

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "S3URL" $ do
    it "parses from JSON" $
        withDecoded "http://localhost:4569/my-bucket" $ \url -> do
            let ep = serviceEndpoint url

            view endpointSecure ep `shouldBe` False
            view endpointHost ep `shouldBe` "localhost"
            view endpointPort ep `shouldBe` 4569

            s3Bucket url `shouldBe` BucketName "my-bucket"

    it "parses from JSON without authority" $
        withDecoded "http:///my-bucket" $ \url -> do
            let ep = serviceEndpoint url

            view endpointSecure ep `shouldBe` False
            view endpointHost ep `shouldBe` "s3.amazonaws.com"
            view endpointPort ep `shouldBe` 80

            s3Bucket url `shouldBe` BucketName "my-bucket"

    it "parses from JSON without port" $
        withDecoded "https://localhost/my-bucket" $ \url -> do
            let ep = serviceEndpoint url

            view endpointSecure ep `shouldBe` True
            view endpointHost ep `shouldBe` "localhost"
            view endpointPort ep `shouldBe` 443

    it "has nice parse errors" $ do
        let Left err1 = eitherDecode "\"ftp://invalid\"" :: Either String S3URL
        let Left err2 = eitherDecode "\"https://localhost\"" :: Either String S3URL

        err1 `shouldEndWith` "Invalid S3 URL: cannot infer port"
        err2 `shouldEndWith` "Invalid S3 URL: bucket not provided"

serviceEndpoint :: S3URL -> Endpoint
serviceEndpoint url = _svcEndpoint (s3Service url) NorthVirginia

withDecoded :: String -> (S3URL -> Expectation) -> Expectation
withDecoded str ex = either failure ex decoded

  where
    decoded = eitherDecode $ C8.pack $ "\"" <> str <> "\""

    failure err = expectationFailure $ unlines
        [ "Expected " <> str <> " to parse as JSON"
        , "Error: " <> err
        ]
