module Handler.Commands where

import Import

postCommandsR :: Handler ()
postCommandsR = do
    token <- (fst . random . appRandomGem) <$> getYesod

    redirect $ CommandR token
