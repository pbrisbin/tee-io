module SpecHelper
    ( module SpecHelper
    , module X
    ) where

import Application           (makeFoundation)
import ClassyPrelude         as X
import Data.Aeson            as X
import Database.Persist      as X hiding (get, delete)
import Foundation            as X
import Network.HTTP.Types    as X
import Network.Wai.Test      as X (SResponse(..))
import Token                 as X
import Model                 as X
import Test.Hspec            as X hiding
    ( expectationFailure
    , shouldBe
    , shouldSatisfy
    , shouldContain
    , shouldMatchList
    , shouldReturn
    )
import Test.Hspec.Expectations.Lifted as X
import Yesod.Default.Config2 (ignoreEnv, loadAppSettings)
import Yesod.Persist         as X (getBy404)
import Yesod.Test            as X

import Database.Persist.Sql  (SqlPersistM, SqlBackend, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Yesod.Core.Handler (RedirectUrl)

withApp :: SpecWith App -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv

    app <- makeFoundation settings
    wipeDB app
    return app

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

delete :: RedirectUrl App url => url -> YesodExample App ()
delete url = request $ do
    setMethod "DELETE"
    setUrl url

postJSON :: (RedirectUrl App url, ToJSON a) => url -> a -> YesodExample App ()
postJSON url body = request $ do
    setMethod "POST"
    addRequestHeader (hAccept, "application/json")
    addRequestHeader (hContentType, "application/json")
    setRequestBody $ encode body
    setUrl url

patchJSON :: (RedirectUrl App url, ToJSON a) => url -> a -> YesodExample App ()
patchJSON url body = request $ do
    setMethod "PUT"
    addRequestHeader (hAccept, "application/json")
    addRequestHeader (hContentType, "application/json")
    setRequestBody $ encode body
    setUrl url

putJSON :: (RedirectUrl App url, ToJSON a) => url -> a -> YesodExample App ()
putJSON url body = request $ do
    setMethod "PUT"
    addRequestHeader (hAccept, "application/json")
    addRequestHeader (hContentType, "application/json")
    setRequestBody $ encode body
    setUrl url

withJSONResponse :: FromJSON a => (a -> YesodExample App ()) -> YesodExample App ()
withJSONResponse f = withResponse $ \rsp -> do
    let bs = simpleBody rsp

    maybe (expectationFailure $ "failed to parse " <> show bs) f $ decode bs

wipeDB :: App -> IO ()
wipeDB app = do
    runDBWithApp app $ do
        tables <- getTables
        sqlBackend <- ask

        let escapedTables = map (connEscapeName sqlBackend . DBName) tables
            query = "TRUNCATE TABLE " ++ (intercalate ", " escapedTables)
        rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public';" []
    return $ map unSingle tables
