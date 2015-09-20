module Storage
    ( Storage
    , StorageError(..)
    , runStorage
    , throwError
    , createCommand
    , updateCommand
    , createOutput
    , getCommandData
    , getOutput
    , unsafeRunStorage
    ) where

import Model
import Foundation

import ClassyPrelude.Yesod
import Control.Monad.Trans.Except
import Data.Aeson hiding (Result)
import Data.UUID
import System.Random

import qualified Data.ByteString.Lazy as BL
import qualified Database.Redis as Redis

-- | Errors that may occur when interacting with storage
data StorageError
    = RedisError Redis.Reply
    | JSONParseError String
    | GenericError String

instance Show StorageError where
    show (RedisError reply) = "Redis error: " <> show reply
    show (JSONParseError err) = "JSON parse error: " <> err
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

-- | Insert the command into storage and return its token
createCommand :: Command -> Storage Token
createCommand command = do
    token <- newToken
    outputToken <- OutputToken <$> newToken

    lift $ $(logDebug) "creating command"
    lift $ $(logDebug) $ "token " <> pack (show token)
    lift $ $(logDebug) $ "output token " <> pack (show outputToken)

    t <- liftIO getCurrentTime
    let commandData = CommandData command outputToken t

    void $ runRedis $ Redis.set (toKey token) $ toValue commandData

    return token

-- | Update a given command
updateCommand :: Token -> Command -> Storage ()
updateCommand token command = do
    cd <- getCommandData token

    lift $ $(logDebug) "updating command"
    lift $ $(logDebug) $ "token " <> pack (show token)

    void $ runRedis $ Redis.set (toKey token) $ toValue cd { cdCommand = command }

-- | Create output for a command
createOutput :: OutputToken -> Output -> Storage ()
createOutput (OutputToken token) output = do
    lift $ $(logDebug) "creating command output"
    lift $ $(logDebug) $ "token " <> pack (show token)
    void $ runRedis $ Redis.rpush (toKey token) [toValue output]

-- | Retrieve the data associated with a command
getCommandData :: Token -> Storage CommandData
getCommandData token = do
    value <- runRedis $ Redis.get $ toKey token

    maybe (lift notFound) parseValue value

-- | Retrieve the outputs associated with a command
getOutput :: OutputToken -> Integer -> Storage [Output]
getOutput (OutputToken token) start = do
    l <- runRedis $ Redis.llen (toKey token)
    bs <- runRedis $ Redis.lrange (toKey token) start l

    mapM parseValue bs

newToken :: Storage Token
newToken = liftIO $ randomIO

toKey :: Token -> ByteString
toKey = encodeUtf8 . toText . tokenUUID

toValue :: ToJSON a => a -> ByteString
toValue = BL.toStrict . encode

parseValue :: FromJSON a => ByteString -> Storage a
parseValue = either (throwE . JSONParseError) return . eitherDecode . fromStrict

runRedis :: Redis.Redis (Either Redis.Reply a) -> Storage a
runRedis a = do
    conn <- lift $ appRedis <$> getYesod
    result <- liftIO $ Redis.runRedis conn a

    either (throwE . RedisError) return result
