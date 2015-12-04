module Worker
    ( workerMain

    -- Exported for testing
    , archivableCommands
    ) where

import Import hiding
    ( (<.)
    , (=.)
    , (==.)
    , (>.)
    , (||.)
    , delete
    , isNothing
    , on
    , update
    )

import Archive
import Application (handler)

import Control.Concurrent (threadDelay)
import Data.Time.Duration
import Database.Esqueleto

workerMain :: IO ()
workerMain = handler $ forever $ do
    $(logInfo) $ "worker loop start"

    timeout <- appCommandTimeout . appSettings <$> getYesod
    archiveCommands timeout

    $(logInfo) $ "worker loop end"
    sleep $ timeout * 2

archiveCommands :: Second -> Handler ()
archiveCommands timeout = runDB $ do
    commands <- archivableCommands timeout

    mapM_ archiveCommand commands

archivableCommands :: MonadIO m => Second -> ReaderT SqlBackend m [Entity Command]
archivableCommands timeout = do
    cutoff <- (timeout `priorTo`) <$> liftIO getCurrentTime
    select $ from $ \(c `LeftOuterJoin` mo) -> do
        on ((just (c ^. CommandId) ==. mo ?. OutputCommand) &&.
            (mo ?. OutputCreatedAt >. just (val cutoff)))
        where_ ((c ^. CommandRunning ==. val True) &&.
                (c ^. CommandCreatedAt <. val cutoff) &&.
                (isNothing $ mo ?. OutputId))
        return c

archiveCommand :: Entity Command -> ReaderT SqlBackend Handler ()
archiveCommand (Entity commandId command) = do
    results <- select $ from $ \o -> do
        where_ (o ^. OutputCommand ==. val commandId)
        return o

    let outputs = map entityVal results
    lift $ archiveOutput (commandToken command) outputs

    delete $ from $ \o ->
        where_ (o ^. OutputCommand ==. val commandId)

    update $ \c -> do
        set c [CommandRunning =. val False]
        where_ (c ^. CommandId ==. val commandId)

    $(logInfo) $ "archived to S3 " <> tokenText (commandToken command)

sleep :: MonadIO m => TimeUnit a => a -> m ()
sleep = liftIO . threadDelay . fromInteger . toMicroseconds
