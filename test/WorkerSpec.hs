module WorkerSpec
    ( main
    , spec
    ) where

import SpecHelper
import Worker
import Data.Time.Duration

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $
    describe "archivableCommands" $ do
        it "does not find recent commands" $ do
            now <- liftIO getCurrentTime
            token <- newToken
            void $ runDB $ insert Command
                { commandToken = token
                , commandDescription = Nothing
                , commandCreatedAt = now
                }

            results <- runDB $ archivableCommands 30
            results `shouldBe` []

        it "does not find old commands with recent output" $ do
            now <- liftIO getCurrentTime
            token <- newToken
            runDB $ do
                commandId <- insert Command
                    { commandToken = token
                    , commandDescription = Nothing
                    , commandCreatedAt = (35 :: Second) `priorTo` now
                    }

                void $ insert Output
                    { outputCommand = commandId
                    , outputContent = ""
                    , outputCreatedAt = now
                    }

            results <- runDB $ archivableCommands 30
            results `shouldBe` []

        it "finds old commands with old or no output" $ do
            now <- liftIO getCurrentTime
            token1 <- newToken
            token2 <- newToken
            runDB $ do
                void $ insert Command
                    { commandToken = token1
                    , commandDescription = Nothing
                    , commandCreatedAt = (45 :: Second) `priorTo` now
                    }

                commandId <- insert Command
                    { commandToken = token2
                    , commandDescription = Nothing
                    , commandCreatedAt = (40 :: Second) `priorTo` now
                    }

                void $ insert Output
                    { outputCommand = commandId
                    , outputContent = ""
                    , outputCreatedAt = (35 :: Second) `priorTo` now
                    }

            results <- runDB $ archivableCommands 30

            length results `shouldBe` 2
