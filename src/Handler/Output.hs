module Handler.Output
    ( postOutputR
    , getOutputR
    ) where

import Import hiding (Request)
import Network.WebSockets (ConnectionException)
import Yesod.WebSockets

data Request = Request
    { reqContent :: Text
    }

instance FromJSON Request where
    parseJSON = withObject "Output.Request" $ \o -> Request
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

getOutputR :: Token -> Handler ()
getOutputR token = do
    Entity commandId _ <- runDB $ getBy404 $ UniqueCommand token

    webSockets $ outputStream commandId 0

outputStream :: CommandId -> Int -> WebSocketsT Handler ()
outputStream commandId start = catchingConnectionException $ do
    outputs <- lift $ runDB $ selectList
        [OutputCommand ==. commandId]
        [Asc OutputCreatedAt, OffsetBy start]

    sendTextDataAck ""
    mapM_ (sendTextDataAck . outputContent . entityVal) outputs

    outputStream commandId (start + length outputs)

catchingConnectionException :: WebSocketsT Handler () -> WebSocketsT Handler ()
catchingConnectionException f = f `catch` \e ->
    $(logDebug) $ pack $ show (e :: ConnectionException)

sendTextDataAck :: MonadIO m => Text -> WebSocketsT m ()
sendTextDataAck msg = do
    sendTextData msg
    void receiveTextData

-- Just fixing the type to Text
receiveTextData :: MonadIO m => WebSocketsT m Text
receiveTextData = receiveData
