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

import Data.Time.Duration
import Database.Esqueleto

import qualified Data.Text as T

workerMain :: IO ()
workerMain = handler $ do
    timeout <- appCommandTimeout . appSettings <$> getYesod
    archiveCommands timeout

archiveCommands :: Second -> Handler ()
archiveCommands timeout = runDB $ do
    commands <- archivableCommands timeout

    $(logInfo) $ "archive_commands count=" <> T.pack (show $ length commands)

    mapM_ archiveCommand commands

archivableCommands :: MonadIO m => Second -> ReaderT SqlBackend m [Entity Command]
archivableCommands timeout = do
    cutoff <- (timeout `priorTo`) <$> liftIO getCurrentTime
    select $ from $ \(c `LeftOuterJoin` mo) -> do
        on ((just (c ^. CommandId) ==. mo ?. OutputCommand) &&.
            (mo ?. OutputCreatedAt >. just (val cutoff)))
        where_ ((c ^. CommandCreatedAt <. val cutoff) &&.
                isNothing (mo ?. OutputId))
        return c

archiveCommand :: Entity Command -> ReaderT SqlBackend Handler ()
archiveCommand (Entity commandId command) = do
    outputs <- commandOutputs commandId 0
    lift $ archiveOutput (commandToken command) outputs

    deleteCommand commandId

    $(logInfo) $ "archived token=" <> tokenText (commandToken command)
