module SpecHelper
    ( module SpecHelper
    , module X
    ) where

import Application           (makeFoundation)
import ClassyPrelude         as X
import Data.Aeson            as X
import Foundation            as X
import Network.HTTP.Types    as X
import Network.Wai.Test      as X (SResponse(..))
import Model                 as X
import Storage               as X hiding (get)
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
import Yesod.Test            as X

import Yesod.Core.Handler (RedirectUrl)

import qualified Storage as Storage

withApp :: SpecWith App -> Spec
withApp = before $ do
    settings <- loadAppSettings
        ["config/settings.yml"]
        []
        ignoreEnv
    makeFoundation settings

runStorage' :: Storage a -> YesodExample App a
runStorage' f = do
    conn <- appRedis <$> getTestYesod
    result <- liftIO $ runStorage conn f

    either err return result

  where
    err x = error $ "storage command failed: " <> show x

getCommand :: Token -> YesodExample App Command
getCommand k = do
    mval <- runStorage' $ Storage.get k
    maybe (error "command not found") return mval

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
