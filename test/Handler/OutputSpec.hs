module Handler.OutputSpec
    ( main
    , spec
    ) where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "POST /commands/token/output" $
        it "creates command output" $ do
            now <- liftIO getCurrentTime
            token <- newToken
            void $ runDB $ insert Command
                    { commandToken = token
                    , commandRunning = True
                    , commandDescription = Nothing
                    , commandCreatedAt = now
                    }

            postJSON (OutputR token) $ object ["content" .= ("line 1\n" :: Text)]
            postJSON (OutputR token) $ object ["content" .= ("line 2\n" :: Text)]
            postJSON (OutputR token) $ object ["content" .= ("line 3\n" :: Text)]

            outputs <- runDB $ selectList [] []
            map (outputContent . entityVal) outputs `shouldBe`
                [ "line 1\n"
                , "line 2\n"
                , "line 3\n"
                ]

    describe "GET /commands/token/output" $
        it "streams output via websockets" $ do
            now <- liftIO getCurrentTime
            token <- newToken
            void $ runDB $ do
                commandId <- insert Command
                    { commandToken = token
                    , commandRunning = True
                    , commandDescription = Nothing
                    , commandCreatedAt = now
                    }

                void $ insert Output
                    { outputCommand = commandId
                    , outputContent = "line 1\n"
                    , outputCreatedAt = now
                    }
                void $ insert Output
                    { outputCommand = commandId
                    , outputContent = "line 2\n"
                    , outputCreatedAt = now
                    }

            -- TODO somehow run the websocket producer with a test client and
            -- assert we get the two lines of output
            get $ OutputR token

            statusIs 200
