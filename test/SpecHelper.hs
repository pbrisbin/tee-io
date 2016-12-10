module SpecHelper
    ( module SpecHelper
    , module X
    ) where

import Application (makeFoundation, makeLogWare)

import Database.Persist.Sql
    ( SqlBackend
    , SqlPersistM
    , connEscapeName
    , rawExecute
    , rawSql
    , runSqlPersistMPool
    , unSingle
    )
import Yesod.Core.Handler (RedirectUrl)
import Yesod.Default.Config2 (loadAppSettings, useEnv)

import Foundation as X
import Token as X
import Model as X

import ClassyPrelude as X
import Data.Aeson as X
import Database.Persist as X hiding (get, delete)
import Network.HTTP.Types as X
import Network.Wai.Test as X (SResponse(..))
import Test.Hspec as X hiding
    ( expectationFailure
    , shouldBe
    , shouldSatisfy
    , shouldContain
    , shouldMatchList
    , shouldReturn
    )
import Test.Hspec.Expectations.Lifted as X
import Text.Shakespeare.Text as X (st)
import Yesod.Persist as X (getBy404)
import Yesod.Test as X

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/settings.yml"]
        []
        useEnv

    app <- makeFoundation settings
    wipeDB app
    logWare <- liftIO $ makeLogWare app
    return (app, logWare)

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
postJSON = requestJSON "POST"

patchJSON :: (RedirectUrl App url, ToJSON a) => url -> a -> YesodExample App ()
patchJSON = requestJSON "PATCH"

putJSON :: (RedirectUrl App url, ToJSON a) => url -> a -> YesodExample App ()
putJSON = requestJSON "PUT"

requestJSON :: (RedirectUrl App url, ToJSON a) => Method -> url -> a -> YesodExample App ()
requestJSON method url body = request $ do
    setMethod method
    addRequestHeader (hAccept, "application/json")
    addRequestHeader (hContentType, "application/json")
    setRequestBody $ encode body
    setUrl url

withJSONResponse :: FromJSON a => (a -> YesodExample App ()) -> YesodExample App ()
withJSONResponse f = withResponse $ \rsp -> do
    let bs = simpleBody rsp

    maybe (expectationFailure $ "failed to parse " <> show bs) f $ decode bs

wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables
