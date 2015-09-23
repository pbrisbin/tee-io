module Handler.OutputSpec
    ( main
    , spec
    ) where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "POST /commands/token/output" $ do
        it "creates command output" $ do
            now <- liftIO $ getCurrentTime
            token <- newToken
            runStorage' $ set token $ Command
                    { commandRunning = True
                    , commandDescription = Nothing
                    , commandCreatedAt = now
                    , commandUpdatedAt = now
                    }

            postJSON (OutputR token) $ object ["content" .= ("line 1\n" :: Text)]
            postJSON (OutputR token) $ object ["content" .= ("line 2\n" :: Text)]
            postJSON (OutputR token) $ object ["content" .= ("line 3\n" :: Text)]

            outputs <- runStorage' $ lget (History token) 0
            map outputContent outputs `shouldBe`
                [ "line 1\n"
                , "line 2\n"
                , "line 3\n"
                ]

    describe "GET /commands/token/output" $ do
        it "streams output via websockets" $ do
            now <- liftIO $ getCurrentTime
            token <- newToken
            runStorage' $ do
                set token $ Command
                    { commandRunning = True
                    , commandDescription = Nothing
                    , commandCreatedAt = now
                    , commandUpdatedAt = now
                    }

                rpush (History token) $ Output
                    { outputContent = "line 1\n"
                    , outputCreatedAt = now
                    }
                rpush (History token) $ Output
                    { outputContent = "line 2\n"
                    , outputCreatedAt = now
                    }

            -- TODO somehow run the websocket prucer with a test client and
            -- assert we get the two lines of output
            get $ OutputR token

            statusIs 200
