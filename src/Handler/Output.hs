module Handler.Output
    ( postOutputR
    , getOutputR
    ) where

import Import hiding (Request)

import Data.List (genericLength)
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

    command <- lift $ unsafeRunStorage $ get404 token
    if (commandRunning command)
        then outputStream token $ start + genericLength outputs
        else sendClose ("command no longer running" :: Text)
