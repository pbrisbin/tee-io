module Handler.CommandSpec
    ( main
    , spec
    ) where

import SpecHelper

data Response = Response Token

instance FromJSON Response where
    parseJSON = withObject "Response" $ \o -> Response
        <$> o .: "token"

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "POST /commands" $ do
        it "creates a new command" $ do
            postJSON CommandsR $ object []

            withJSONResponse $ \(Response token) -> do
                command <- runStorage' $ get404 token
                commandRunning command `shouldBe` True
                commandDescription command `shouldBe` Nothing

        it "creates a command with a description" $ do
            postJSON CommandsR $ object ["description" .= ("test command" :: Text)]

            withJSONResponse $ \(Response token) -> do
                command <- runStorage' $ get404 token
                commandRunning command `shouldBe` True
                commandDescription command `shouldBe` Just "test command"

    describe "PUT /commands/token" $ do
        it "sets commandUpdatedAt and preserves existing fields" $ do
            now <- liftIO $ getCurrentTime
            token <- newToken
            runStorage' $ set token $ Command
                { commandRunning = True
                , commandDescription = Just "a description"
                , commandCreatedAt = now
                , commandUpdatedAt = now
                }

            putJSON (CommandR token) $ object ["running" .= False]

            updated <- runStorage' $ get404 token
            commandRunning updated `shouldBe` False
            commandDescription updated `shouldBe` Just "a description"
            commandUpdatedAt updated `shouldSatisfy` (not . (== now))
