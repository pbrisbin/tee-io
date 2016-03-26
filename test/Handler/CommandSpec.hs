module Handler.CommandSpec
    ( main
    , spec
    ) where

import SpecHelper
import Data.UUID (fromText)

data Response = Response Token

instance FromJSON Response where
    parseJSON = withObject "Response" $ \o -> Response
        <$> (parseToken =<< o .: "token")

      where
        parseToken = maybe mzero (return . Token) . fromText

main :: IO ()
main = hspec spec

spec :: Spec
spec = withApp $ do
    describe "POST /commands" $ do
        it "creates a new command" $ do
            postJSON CommandsR $ object []

            withJSONResponse $ \(Response token) -> do
                Entity _ command <- runDB $ getBy404 $ UniqueCommand token
                commandDescription command `shouldBe` Nothing

        it "creates a command with a description" $ do
            postJSON CommandsR $ object ["description" .= ("test command" :: Text)]

            withJSONResponse $ \(Response token) -> do
                Entity _ command <- runDB $ getBy404 $ UniqueCommand token
                commandDescription command `shouldBe` Just "test command"

    describe "DELETE /commands/token" $
        it "deletes the command's data" $ do
            now <- liftIO getCurrentTime
            token <- newToken
            void $ runDB $ insert Command
                { commandToken = token
                , commandRunning = True
                , commandDescription = Just "a description"
                , commandCreatedAt = now
                }

            delete $ CommandR token
            statusIs 200

            results <- runDB $ selectList [CommandToken ==. token] []
            results `shouldBe` []
