module Model where

import Token
import ClassyPrelude.Yesod
import Database.Persist.Quasi

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

exists
    :: ( MonadIO m
       , PersistQuery (PersistEntityBackend v)
       , PersistEntity v
       )
    => [Filter v] -> ReaderT (PersistEntityBackend v) m Bool
exists = fmap (> 0) . count
