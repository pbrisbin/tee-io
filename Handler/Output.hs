module Handler.Output where

import Import

import Data.List (genericLength)
import Yesod.WebSockets

postOutputR :: Token -> Handler ()
postOutputR token = do
    body <- requireJsonBody

    runStorage' $ do
        cd <- getCommandData token
        createOutput (cdOutputToken cd) body

getOutputR :: Token -> Handler ()
getOutputR token = do
    cd <- runStorage' $ getCommandData token
    webSockets $ outputStream token (cdOutputToken cd) 0

outputStream :: Token -> OutputToken -> Integer -> WebSocketsT Handler ()
outputStream token outputToken start = do
    outputs <- lift $ runStorage' $ getOutput outputToken start

    forM_ outputs $ \output -> do
        sendTextData $ outputContent output

        ack <- receiveData -- ensure someone's listening
        $(logDebug) $ "received acknowledgement " <> ack

    cd <- lift $ runStorage' $ getCommandData token
    if (commandRunning $ cdCommand cd)
        then outputStream token outputToken $ start + genericLength outputs
        else sendClose ("command no longer running" :: Text)

runStorage' :: Storage a -> Handler a
runStorage' = either (error . show) return <=< runStorage
