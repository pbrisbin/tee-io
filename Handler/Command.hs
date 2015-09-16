module Handler.Command where

import Import
import qualified Database.Redis as Redis

getCommandR :: Token -> Handler Value
getCommandR = redisGet404

redisGet404 :: FromJSON a => Token -> Handler a
redisGet404 token = do
    result <- runRedis $ Redis.get (tokenToBS token)

    maybe notFound return
        $ (decode . fromStrict)
        =<< (join $ eitherToMaybe result)

eitherToMaybe :: Either t a -> Maybe a
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _) = Nothing
