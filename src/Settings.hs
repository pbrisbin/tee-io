module Settings where

import ClassyPrelude.Yesod hiding (throw)
import Control.Exception (throw)
import Data.Aeson (Result(..), fromJSON, withObject)
import Data.Aeson.Types (Parser)
import Data.FileEmbed (embedFile)
import Data.Time.Units (Second)
import Data.Yaml (decodeEither')
import Database.Persist.Postgresql (PostgresConf(..))
import Language.Haskell.TH.Syntax (Exp, Q)
import Network.AWS (Service)
import Network.AWS.S3 (BucketName(..))
import Network.S3URL (S3URL(..))
import Network.PGDatabaseURL (parsePGConnectionString)
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Default.Config2 (applyEnvValue, configSettingsYml)
import Yesod.Default.Util
#if DEVELOPMENT
    (widgetFileReload)
#else
    (widgetFileNoReload)
#endif

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8

data AppSettings = AppSettings
    { appStaticDir :: String
    , appDatabaseConf :: PostgresConf
    , appRoot :: Text
    , appHost :: HostPreference
    , appPort :: Int
    , appIpFromHeader :: Bool
    , appCommandTimeout :: Second
    , appS3Service :: Service
    , appS3Bucket :: BucketName
    , appLogLevel :: LogLevel
    , appMutableStatic :: Bool
    }

instance Show AppSettings where
    show AppSettings{..} = concat
        [ "log_level=", show appLogLevel
        , " host=", show appHost
        , " port=", show appPort
        , " root=", show appRoot
        , " db=[", C8.unpack $ pgConnStr appDatabaseConf, "]"
        , " s3_bucket=", (\(BucketName t) -> T.unpack t) appS3Bucket
        , " command_timeout=", show appCommandTimeout
        ]

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        url <- o .: "database-url"
        connStr <- either fail return $ parsePGConnectionString url

        appDatabaseConf <- PostgresConf
            <$> pure connStr
            <*> o .: "database-pool-size"
        appRoot <- o .: "approot"
        appHost <- fromString <$> o .: "host"
        appPort <- o .: "port"
        appIpFromHeader <- o .: "ip-from-header"
        appCommandTimeout <- fromIntegral
            <$> (o .: "command-timeout" :: Parser Integer)
        S3URL appS3Service appS3Bucket <- o .: "s3-url"
        appLogLevel <- parseLogLevel <$> o .: "log-level"
        appMutableStatic <- o .: "mutable-static"

        -- This value is needed in a pure context, and so can't read from ENV.
        -- It also doesn't differ between environments, so we might as well
        -- harcode it.
        let appStaticDir = "static"

        return AppSettings{..}

      where
        parseLogLevel :: Text -> LogLevel
        parseLogLevel t = case T.toLower t of
            "debug" -> LevelDebug
            "info" -> LevelInfo
            "warn" -> LevelWarn
            "error" -> LevelError
            _ -> LevelOther t

allowsLevel :: AppSettings -> LogLevel -> Bool
allowsLevel AppSettings{..} = (>= appLogLevel)

widgetFile :: String -> Q Exp
widgetFile =
#if DEVELOPMENT
    widgetFileReload
#else
    widgetFileNoReload
#endif
    def

configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

configSettingsYmlValue :: Value
configSettingsYmlValue = either throw id $ decodeEither' configSettingsYmlBS

compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings
