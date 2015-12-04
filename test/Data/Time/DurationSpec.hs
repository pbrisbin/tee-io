module Data.Time.DurationSpec
    ( main
    , spec
    ) where

import Prelude
import Test.Hspec
import Data.Time.Duration

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Data.Time.Duration" $ do
    describe "since" $ it "works" $ do
        let t1 = mkTime "2015-12-31 05:15:00"
            t2 = (6 :: Second) `since` t1

        t2 `shouldBe` mkTime "2015-12-31 05:15:06"

    describe "priorTo" $ it "works" $ do
        let t1 = mkTime "2015-12-31 05:15:00"
            t2 = (2 :: Day) `priorTo` t1

        t2 `shouldBe` mkTime "2015-12-29 05:15:00"

mkTime :: String -> UTCTime
mkTime = parseTimeOrError False defaultTimeLocale "%F %T"
