module Handler.Commands where

import Import

postCommandsR :: Handler Text
postCommandsR = do
    body <- requireJsonBody

    tokenText <$> unsafeRunStorage (createCommand body)
