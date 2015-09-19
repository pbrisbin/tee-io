module Storage
    ( Storage
    , StorageError(..)
    , runStorage
    , throwError
    , createCommand
    , createOutput
    , getCommandData
    , getOutputs
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

-- | Throw a generic @'StorageError'@
throwError :: String -> Storage a
throwError = throwE . GenericError

-- | Insert the command into storage and return its token
createCommand :: Command -> Storage Token
createCommand command = do
    lift $ $(logDebug) "creating command"
    token <- newToken
    outputToken <- OutputToken <$> newToken

    lift $ $(logDebug) $ "token " <> pack (show token)
    lift $ $(logDebug) $ "output token " <> pack (show outputToken)

    let commandData = CommandData command outputToken

    void $ runRedis $ Redis.set (toKey token) $ toValue commandData

    return token

-- | Create output for a command
createOutput :: OutputToken -> Output -> Storage ()
createOutput (OutputToken token) output = do
    lift $ $(logDebug) "creating command output"
    lift $ $(logDebug) $ "token " <> pack (show token)
    void $ runRedis $ Redis.rpush (toKey token) [toValue output]

-- | Retrieve the data associated with a command
getCommandData :: Token -> Storage CommandData
getCommandData token = do
    lift $ $(logDebug) "getting command data"
    lift $ $(logDebug) $ "token " <> pack (show token)
    value <- runRedis $ Redis.get $ toKey token

    maybe (lift notFound) parseValue value

-- | Retrieve the outputs associated with a command
getOutputs
    :: OutputToken
    -> Maybe Integer -- start entry
    -> Maybe Integer -- stop entry
    -> Storage [Output]
getOutputs (OutputToken token) (Just start) (Just stop) = do
    lift $ $(logDebug) "getting command outputs"
    bs <- runRedis $ Redis.lrange (toKey token) start stop

    mapM parseValue bs

getOutputs ot@(OutputToken token) x Nothing = do
    len <- runRedis $ Redis.llen (toKey token)

    getOutputs ot x (Just len)

getOutputs token Nothing x = getOutputs token (Just 0) x

newToken :: Storage Token
--newToken = lift $ fst . random . appRandomGem <$> getYesod
newToken = liftIO $ randomIO

toKey :: Token -> ByteString
toKey = encodeUtf8 . toText . tokenUUID

toValue :: ToJSON a => a -> ByteString
toValue = BL.toStrict . encode

parseValue :: FromJSON a => ByteString -> Storage a
parseValue = either (throwError) return . eitherDecode . fromStrict

runRedis :: Redis.Redis (Either Redis.Reply a) -> Storage a
runRedis a = do
    conn <- lift $ appRedis <$> getYesod
    result <- liftIO $ Redis.runRedis conn a

    either (throwE . RedisError) return result
