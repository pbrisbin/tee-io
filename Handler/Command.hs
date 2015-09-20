module Handler.Command where

import Import

getCommandR :: Token -> Handler Html
getCommandR token = do
    cd <- unsafeRunStorage $ getCommandData token

    defaultLayout $ do
        setTitle "tee.io - Command"
        $(widgetFile "command")

putCommandR :: Token -> Handler ()
putCommandR token = do
    command <- requireJsonBody
    void $ unsafeRunStorage $ updateCommand token command
