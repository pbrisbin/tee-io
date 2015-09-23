module Handler.Command
    ( postCommandsR
    , putCommandR
    , getCommandR
    )
    where

import Import hiding (Request)

data Request = Request
    { reqRunning :: Maybe Bool
    , reqDescription :: Maybe Text
    }

instance FromJSON Request where
    parseJSON = withObject "Command.Request" $ \o -> Request
        <$> o .:? "running"
        <*> o .:? "description"

postCommandsR :: Handler TypedContent
postCommandsR = do
    now <- liftIO getCurrentTime
    req <- requireJsonBody
    token <- newToken

    unsafeRunStorage $ set token $ Command
        { commandRunning = fromMaybe True $ reqRunning req
        , commandDescription = reqDescription req
        , commandCreatedAt = now
        , commandUpdatedAt = now
        }

    selectRep $ do
        provideRep $ return $ tokenText token
        provideRep $ return $ object ["token" .= tokenText token]

putCommandR :: Token -> Handler ()
putCommandR token = do
    now <- liftIO getCurrentTime
    req <- requireJsonBody

    unsafeRunStorage $ do
        command <- get404 token

        let running = fromMaybe (commandRunning command) $ reqRunning req
            description = maybe (commandDescription command) Just $ reqDescription req

        set token $ command
            { commandRunning = running
            , commandDescription = description
            , commandUpdatedAt = now
            }

getCommandR :: Token -> Handler TypedContent
getCommandR token = do
    command <- unsafeRunStorage $ get404 token

    selectRep $ do
        provideRep $ return $ toJSON command
        provideRep $ defaultLayout $ do
            setTitle "tee.io - Command"
            $(widgetFile "command")
