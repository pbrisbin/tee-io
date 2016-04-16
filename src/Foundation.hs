module Foundation where

import Import.NoFoundation

import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Core.Types (Logger)
import Yesod.Default.Util (addStaticContentExternal)

import qualified Network.AWS as AWS
import qualified Yesod.Core.Unsafe as Unsafe

data App = App
    { appSettings :: AppSettings
    , appStatic :: Static
    , appConnPool :: ConnectionPool
    , appHttpManager :: Manager
    , appLogger :: Logger
    , appAWSEnv :: AWS.Env
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . appSettings

    makeSessionBackend _ = Just <$>
        defaultClientSessionBackend 120 "config/client_session_key.aes"

    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_screen_css
            $(widgetFile "default-layout")

        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        genFileName lbs = "autogen-" ++ base64md5 lbs

    shouldLog App{..} _source = (appSettings `allowsLevel`)

    makeLogger = return . appLogger

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
