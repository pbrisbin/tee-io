module Handler.Commands where

import Import
import qualified Data.ByteString.Lazy as BL
import qualified Database.Redis as Redis

postCommandsR :: Handler ()
postCommandsR = do
    token <- (fst . random . appRandomGem) <$> getYesod

    let json = BL.toStrict $ encode $ newCommand
    void $ runRedis $ Redis.set (tokenToBS token) json

    redirect $ CommandR token
