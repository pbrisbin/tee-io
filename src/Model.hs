module Model where

import Token
import ClassyPrelude.Yesod
import Database.Persist.Quasi

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

exists
    :: ( MonadIO m
       , PersistQueryRead b
       , PersistRecordBackend v b
       , PersistEntity v
       )
    => [Filter v] -> ReaderT b m Bool
exists = fmap (> 0) . count

commandOutputs :: MonadIO m => CommandId -> Int -> ReaderT SqlBackend m [Output]
commandOutputs commandId start = map entityVal <$> selectList
    [OutputCommand ==. commandId]
    [Asc OutputCreatedAt, OffsetBy start]

deleteCommand :: MonadIO m => CommandId -> ReaderT SqlBackend m ()
deleteCommand commandId = do
    deleteWhere [OutputCommand ==. commandId]
    delete commandId
