module Handler.Output
    ( postOutputR
    , getOutputR
    ) where

import Import hiding (Request)
import Archive

import Data.List (genericLength)
import Data.Time (diffUTCTime)
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

    let output = Output
            { outputContent = reqContent req
            , outputCreatedAt = now
            }

    unsafeRunStorage $ do
        void $ (get404 token :: Storage Command)
        void $ rpush (History token) output

getOutputR :: Token -> Handler ()
getOutputR token = webSockets $ outputStream token 0

outputStream :: Token -> Integer -> WebSocketsT Handler ()
outputStream token start = do
    outputs <- lift $ unsafeRunStorage $ lget (History token) start

    forM_ outputs $ \output -> do
        sendTextData $ outputContent output

        ack <- receiveData -- ensure someone's listening
        $(logDebug) $ "received acknowledgement " <> ack

    now <- liftIO $ getCurrentTime
    command <- lift $ unsafeRunStorage $ get404 token

    if not $ stale now command
        then outputStream token $ start + genericLength outputs
        else do
            lift $ archiveOutput command
            sendClose ("command no longer running" :: Text)

  where
    -- command has been stopped for more than 10 minutes
    stale :: UTCTime -> Command -> Bool
    stale t Command{..} = not commandRunning &&
        diffUTCTime t commandUpdatedAt > 10 * 60

    archiveOutput :: Command -> Handler ()
    archiveOutput command = do
        putOutput token =<< unsafeRunStorage (lget (History token) 0)

        unsafeRunStorage $ do
            set token $ command { commandArchived = True }
            del $ History token
