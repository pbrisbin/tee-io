module Storage
    ( Storage
    , StorageError(..)
    , runStorage
    , unsafeRunStorage
    , throwError
    , StorageKey(..)
    , HistoryToken(..)
    , newToken
    , get404
    , set
    , del
    , lget
    , rpush
    ) where

import Model
import Foundation

import ClassyPrelude.Yesod hiding (get404)
import Control.Monad.Trans.Except
import Data.Aeson hiding (Result)
import Data.UUID
import System.Random

import qualified Data.ByteString.Lazy as BL
import qualified Database.Redis as Redis

-- | Errors that may occur when interacting with storage
data StorageError
    = RedisError Redis.Reply
    | JSONParseError ByteString String
    | GenericError String

instance Show StorageError where
    show (RedisError reply) = "Redis error: " <> show reply
    show (JSONParseError bs err) = unlines
        [ "JSON parse error"
        , "source: " <> show bs
        , "error: " <> err
        ]
    show (GenericError err) = "Error: " <> err

-- | @'ExceptT'@ fixed at @'StorageError'@ and @'Handler'@
type Storage = ExceptT StorageError Handler

-- | Run a @'Storage'@ computation for an @'Either'@ result
runStorage :: Storage a -> Handler (Either StorageError a)
runStorage = runExceptT

-- | Run a @'Storage@' computation in an unsafe way
--
-- Errors are logged and raised with @'error'@
unsafeRunStorage :: Storage a -> Handler a
unsafeRunStorage = either (err . show) return <=< runStorage
  where
    err msg = $(logError) (pack msg) >> error msg

-- | Throw a generic @'StorageError'@
throwError :: String -> Storage a
throwError = throwE . GenericError

class StorageKey a where
    toKey :: a -> ByteString

instance StorageKey Token where
    toKey = encodeUtf8 . toText . tokenUUID

newtype HistoryToken = History Token deriving (Show, FromJSON, ToJSON)

instance StorageKey HistoryToken where
    toKey (History t) = toKey t <> ".history"

-- | A version of @get@ that returns @'notFound'@ when missing
get404 :: (StorageKey k, FromJSON a) => k -> Storage a
get404 token = do
    value <- runRedis $ Redis.get $ toKey token

    maybe (lift notFound) parseValue value

-- | Pass-through to @set@
set :: (StorageKey k, ToJSON a) => k -> a -> Storage ()
set token = void . runRedis . Redis.set (toKey token) . toValue

-- | Pass-through to @delete@
del :: (StorageKey k) => k -> Storage ()
del token = void $ runRedis $ Redis.del [toKey token]

-- | Combine @llen@ and @lrange@ to get all elements in a list
lget :: (StorageKey k, FromJSON a) => k -> Integer -> Storage [a]
lget k start = do
    l <- runRedis $ Redis.llen (toKey k)
    bs <- runRedis $ Redis.lrange (toKey k) start l

    mapM parseValue bs

-- | A verison of @rpush@ that takes a single element
rpush :: (StorageKey k, ToJSON a) => k -> a -> Storage ()
rpush k v = void $ runRedis $ Redis.rpush (toKey k) $ [toValue v]

newToken :: MonadIO m => m Token
newToken = liftIO $ randomIO

toValue :: ToJSON a => a -> ByteString
toValue = BL.toStrict . encode

parseValue :: FromJSON a => ByteString -> Storage a
parseValue v = either (throwE . JSONParseError v) return
    $ eitherDecode $ fromStrict v

runRedis :: Redis.Redis (Either Redis.Reply a) -> Storage a
runRedis a = do
    conn <- lift $ appRedis <$> getYesod
    result <- liftIO $ Redis.runRedis conn a

    either (throwE . RedisError) return result
