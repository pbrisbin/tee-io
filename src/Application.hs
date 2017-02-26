{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( appMain
    , develMain
    , makeFoundation
    , makeLogWare
    , getApplicationRepl
    , shutdownApp
    , handler
    , db
    ) where

import Import

import Handler.Common
import Handler.Home
import Handler.Command
import Handler.Output

import Control.Lens (set)
import Control.Monad.Logger (liftLoc, runLoggingT)
import Database.Persist.Postgresql
    ( createPostgresqlPool
    , pgConnStr
    , pgPoolSize
    , runSqlPool
    )
import Language.Haskell.TH.Syntax (qLocation)
import LoadEnv (loadEnv)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
    ( Settings
    , defaultSettings
    , defaultShouldDisplayException
    , runSettings
    , setHost
    , setOnException
    , setPort
    , getPort
    )
import Network.Wai.Middleware.RequestLogger
    ( Destination(Callback, Logger)
    , IPAddrSource(..)
    , OutputFormat(..)
    , destination
    , mkRequestLogger
    , outputFormat
    )
import System.Log.FastLogger
    ( defaultBufSize
    , newStdoutLoggerSet
    , toLogStr
    )

import qualified Data.Text as T
import qualified Network.AWS as AWS

mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    appAWSEnv <- newAWSEnv $ appSettings `allowsLevel` LevelDebug
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    -- Bootstrap a log function to use when creating the connection pool
    let mkFoundation appConnPool = App{..}
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    -- Perform database migrations
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Output settings at startup
    runLoggingT ($(logInfo) $ "settings " <> T.pack (show appSettings)) logFunc

    return $ mkFoundation pool

  where
    newAWSEnv debug = AWS.configure (appS3Service appSettings) <$> do
        logger <- AWS.newLogger (if debug then AWS.Debug else AWS.Error) stdout
        set AWS.envLogger logger <$> AWS.newEnv AWS.Discover

makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation = mkRequestLogger def
    { outputFormat = if appSettings foundation `allowsLevel` LevelDebug
        then Detailed True
        else Apache apacheIpSource
    , destination = if appSettings foundation `allowsLevel` LevelInfo
        then Logger $ loggerSet $ appLogger foundation
        else Callback $ \_ -> return ()
    }
  where
    apacheIpSource = if appIpFromHeader $ appSettings foundation
        then FromFallback
        else FromSocket

warpSettings :: App -> Settings
warpSettings foundation =
    setPort (appPort $ appSettings foundation) $
    setHost (appHost $ appSettings foundation) $
    setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

getAppSettings :: IO AppSettings
getAppSettings = do
    loadEnv
    loadYamlSettings [configSettingsYml] [] useEnv

develMain :: IO ()
develMain = develMainHelper $ do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

appMain :: IO ()
appMain = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    app <- makeApplication foundation

    runSettings (warpSettings foundation) app

--------------------------------------------------------------------------------
-- Functions for the REPL
--------------------------------------------------------------------------------

getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()

handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB
