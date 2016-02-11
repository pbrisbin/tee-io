module Handler.Command
    ( postCommandsR
    , patchCommandR
    , getCommandR
    , deleteCommandR
    , putCommandR
    )
    where

import Import
import Archive
import CommandContent

import Network.AWS (catching)
import Network.AWS.S3 (_NoSuchKey)

data CommandRequest = CommandRequest
    { reqRunning :: Bool
    , reqDescription :: Maybe Text
    }

instance FromJSON CommandRequest where
    parseJSON = withObject "CommandRequest" $ \o -> CommandRequest
        <$> o .:? "running" .!= True
        <*> o .:? "description"

postCommandsR :: Handler TypedContent
postCommandsR = do
    now <- liftIO getCurrentTime
    req <- requireJsonBody
    token <- newToken

    void $ runDB $ insert Command
        { commandToken = token
        , commandRunning = reqRunning req
        , commandDescription = reqDescription req
        , commandCreatedAt = now
        }

    selectRep $ do
        provideRep (sendResponseStatus status201 $ tokenText token :: Handler Text)
        provideRep (sendResponseStatus status201 $ object ["token" .= token] :: Handler Value)
        provideRep (redirect $ CommandR token :: Handler Html)

getCommandR :: Token -> Handler Html
getCommandR token = do
    content <- runDB $ findContent404 token

    defaultLayout $ do
        setTitle "tee.io - Command"
        $(widgetFile "command")

deleteCommandR :: Token -> Handler ()
deleteCommandR token = do
    runDB $ mapM_ (deleteCommand . entityKey) =<< getBy (UniqueCommand token)
    catching _NoSuchKey (deleteArchivedOutput token) $ \_ -> return ()

-- Deprecated. Originally we required callers to update commands to
-- running:false so we could take steps to archive content to S3. We'll instead
-- implement timeout semantics.
patchCommandR :: Token -> Handler ()
patchCommandR _ = return ()

-- Deprecated. Originally wrote the API to accept PUT with PATCH semantics. We
-- still except it for older clients.
putCommandR :: Token -> Handler ()
putCommandR _ = return ()
