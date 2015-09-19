module Handler.Output where

import Import

import Data.List (genericLength)
import Yesod.WebSockets

postOutputR :: Token -> Handler ()
postOutputR token = do
    body <- requireJsonBody

    runStorage' $ do
        cd <- getCommandData token

        if commandRunning $ cdCommand cd
            then createOutput (cdOutputToken cd) body
            else throwError "command not running"

getOutputR :: Token -> Handler ()
getOutputR token = do
    cd <- runStorage' $ getCommandData token
    start <- runInputGet $ ireq intField "start"

    webSockets $ outputStream token (cdOutputToken cd) start

outputStream :: Token -> OutputToken -> Integer -> WebSocketsT Handler ()
outputStream token outputToken start = do
    outputs <- lift $ runStorage' $
        getOutputs outputToken (Just start) Nothing

    forM_ outputs $ \output -> do
        sendTextData $ outputContent output

        ack <- receiveData -- ensure someone's listening
        $(logDebug) $ "received acknowledgement " <> ack

    cd <- lift $ runStorage' $ getCommandData token
    when (commandRunning $ cdCommand cd) $
        outputStream token outputToken $ start + genericLength outputs

runStorage' :: Storage a -> Handler a
runStorage' = either (error . show) return <=< runStorage
