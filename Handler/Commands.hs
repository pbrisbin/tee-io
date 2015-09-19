module Handler.Commands where

import Import

postCommandsR :: Handler Value
postCommandsR = do
    body <- requireJsonBody
    result <- runStorage $ createCommand body

    case result of
        Left err -> error $ show err -- TODO
        Right token -> return $ object ["token" .= token]
