module Handler.Command where

import Import

getCommandR :: Token -> Handler Html
getCommandR token = do
    void $ runStorage $ getCommandData token

    defaultLayout $ do
        setTitle "tee.io - Command"
        $(widgetFile "command")

putCommandR :: Token -> Handler ()
putCommandR token = do
    command <- requireJsonBody
    void $ runStorage $ updateCommand token command
