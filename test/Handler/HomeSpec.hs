module Handler.HomeSpec
    ( main
    , spec
    ) where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    it "loads successfully" $ do
        get HomeR
        statusIs 200
