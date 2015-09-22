module Handler.Command
    ( postCommandsR
    , putCommandR
    , getCommandR
    )
    where

import Import hiding (Request)

data Request = Request
    { reqRunning :: Bool
    , reqDescription :: Maybe Text
    }

instance FromJSON Request where
    parseJSON = withObject "Command.Request" $ \o -> Request
        <$> o .:? "running" .!= True
        <*> o .:? "desctription"

-- TODO Text/Value
postCommandsR :: Handler Text
postCommandsR = do
    now <- liftIO getCurrentTime
    req <- requireJsonBody
    token <- newToken

    unsafeRunStorage $ set token $ Command
        { commandRunning = reqRunning req
        , commandDescription = reqDescription req
        , commandCreatedAt = now
        , commandUpdatedAt = now
        }

    return $ tokenText token

putCommandR :: Token -> Handler ()
putCommandR token = do
    now <- liftIO getCurrentTime
    req <- requireJsonBody

    unsafeRunStorage $ do
        command <- get404 token

        set token $ command
            { commandRunning = reqRunning req
            , commandDescription = reqDescription req
            , commandUpdatedAt = now
            }

-- TODO Html/JSON
getCommandR :: Token -> Handler Html
getCommandR token = do
    command <- unsafeRunStorage $ get404 token

    defaultLayout $ do
        setTitle "tee.io - Command"
        $(widgetFile "command")
