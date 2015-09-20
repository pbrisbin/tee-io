module Handler.Output where

import Import

import Data.List (genericLength)
import Yesod.WebSockets

postOutputR :: Token -> Handler ()
postOutputR token = do
    body <- requireJsonBody

    unsafeRunStorage $ do
        cd <- getCommandData token
        createOutput (cdOutputToken cd) body

getOutputR :: Token -> Handler ()
getOutputR token = do
    cd <- unsafeRunStorage $ getCommandData token
    webSockets $ outputStream token (cdOutputToken cd) 0

outputStream :: Token -> OutputToken -> Integer -> WebSocketsT Handler ()
outputStream token outputToken start = do
    outputs <- lift $ unsafeRunStorage $ getOutput outputToken start

    forM_ outputs $ \output -> do
        sendTextData $ outputContent output

        ack <- receiveData -- ensure someone's listening
        $(logDebug) $ "received acknowledgement " <> ack

    cd <- lift $ unsafeRunStorage $ getCommandData token
    if (commandRunning $ cdCommand cd)
        then outputStream token outputToken $ start + genericLength outputs
        else sendClose ("command no longer running" :: Text)
