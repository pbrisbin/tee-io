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
                commandArchived command `shouldBe` False

        it "creates a command with a description" $ do
            postJSON CommandsR $ object ["description" .= ("test command" :: Text)]

            withJSONResponse $ \(Response token) -> do
                command <- runStorage' $ get404 token
                commandRunning command `shouldBe` True
                commandDescription command `shouldBe` Just "test command"

    describe "PATCH /commands/token" $ do
        it "sets commandUpdatedAt and preserves existing fields" $ do
            now <- liftIO $ getCurrentTime
            token <- newToken
            runStorage' $ set token $ Command
                { commandRunning = True
                , commandDescription = Just "a description"
                , commandArchived = False
                , commandCreatedAt = now
                , commandUpdatedAt = now
                }

            patchJSON (CommandR token) $ object ["running" .= False]

            updated <- runStorage' $ get404 token
            commandRunning updated `shouldBe` False
            commandDescription updated `shouldBe` Just "a description"
            commandUpdatedAt updated `shouldSatisfy` (not . (== now))

    describe "DELETE /commands/token" $ do
        it "404's for non-existent commands" $ do
            token <- newToken

            delete $ CommandR token

            statusIs 404

        it "deletes the command's data" $ do
            now <- liftIO $ getCurrentTime
            token <- newToken
            runStorage' $ set token $ Command
                { commandRunning = True
                , commandDescription = Just "a description"
                , commandArchived = False
                , commandCreatedAt = now
                , commandUpdatedAt = now
                }

            delete $ CommandR token
            statusIs 200

            get $ CommandR token
            statusIs 404
