module Handler.Output
    ( postOutputR
    , getOutputR
    ) where

import Import

import Network.WebSockets (ConnectionException)
import Yesod.WebSockets

data OutputRequest = OutputRequest
    { reqContent :: Text
    }

instance FromJSON OutputRequest where
    parseJSON = withObject "OutputRequest" $ \o -> OutputRequest
        <$> o .: "content"

postOutputR :: Token -> Handler ()
postOutputR token = do
    now <- liftIO getCurrentTime
    req <- requireJsonBody
    void $ runDB $ do
        Entity commandId _ <- getBy404 $ UniqueCommand token

        insert Output
            { outputCommand = commandId
            , outputContent = reqContent req
            , outputCreatedAt = now
            }

    sendResponseStatus status201 ()

getOutputR :: Token -> Handler ()
getOutputR token = do
    Entity commandId _ <- runDB $ getBy404 $ UniqueCommand token

    webSockets $ outputStream commandId 0

outputStream :: CommandId -> Int -> WebSocketsT Handler ()
outputStream commandId start = catchingConnectionException $ do
    outputs <- lift $ runDB $ commandOutputs commandId start

    -- if we get no (more) output, check if the command is still live
    stop <- return (null outputs) &&^ not <$> commandExists

    unless stop $ do
        sendTextDataAck ""
        mapM_ (sendTextDataAck . outputContent) outputs

        outputStream commandId (start + length outputs)

  where
    commandExists = lift $ runDB $ exists [CommandId ==. commandId]

catchingConnectionException :: WebSocketsT Handler () -> WebSocketsT Handler ()
catchingConnectionException f = f `catch` \e ->
    $(logWarn) $ pack $ show (e :: ConnectionException)

sendTextDataAck :: MonadIO m => Text -> WebSocketsT m ()
sendTextDataAck msg = do
    sendTextData msg
    void receiveTextData

-- Just fixing the type to Text
receiveTextData :: MonadIO m => WebSocketsT m Text
receiveTextData = receiveData

-- In this context, it's important the second action isn't evaluated if the
-- first gives @False@ (otherwise we'd make a needless DB query every output
-- iteration). That rules out most existing implementations, including obvious
-- ones like @liftM2 (&&)@.
(&&^) :: Monad m => m Bool -> m Bool -> m Bool
ma &&^ mb = ma >>= \a -> if a then mb else return False
