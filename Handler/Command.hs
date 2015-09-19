module Handler.Command where

import Import

getCommandR :: Token -> Handler Html
getCommandR token = do
    result <- runStorage $ do
        cd <- getCommandData token
        getOutputs (cdOutputToken cd) Nothing Nothing

    case result of
        Left err -> error $ show err -- TODO
        Right outputs -> defaultLayout $ do
            setTitle "tee.io - Command"
            $(widgetFile "command")
