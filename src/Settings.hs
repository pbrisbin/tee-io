module Settings where

import ClassyPrelude.Yesod
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
import Network.Wai.Handler.Warp (HostPreference)
import Web.Heroku.Persist.Postgresql (fromDatabaseUrl)
import Yesod.Default.Config2 (applyEnvValue, configSettingsYml)
import Yesod.Default.Util (widgetFileNoReload, widgetFileReload)

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
    , appDebug :: Bool
    , appReloadTemplates :: Bool
    , appMutableStatic :: Bool
    }

instance Show AppSettings where
    show AppSettings{..} = concat
        [ "debug=", show appDebug
        , " host=", show appHost
        , " port=", show appPort
        , " root=", show appRoot
        , " db=[", C8.unpack $ pgConnStr appDatabaseConf, "]"
        , " s3_bucket=", (\(BucketName t) -> T.unpack t) appS3Bucket
        , " command_timeout=", show appCommandTimeout
        ]

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        appStaticDir <- o .: "static-dir"
        appDatabaseConf <- fromDatabaseUrl
            <$> o .: "database-pool-size"
            <*> o .: "database-url"
        appRoot <- o .: "approot"
        appHost <- fromString <$> o .: "host"
        appPort <- o .: "port"
        appIpFromHeader <- o .: "ip-from-header"
        appCommandTimeout <- fromIntegral
            <$> (o .: "command-timeout" :: Parser Integer)
        S3URL appS3Service appS3Bucket <- o .: "s3-url"
        appDebug <- o .: "debug"
        appReloadTemplates <- o .: "reload-templates"
        appMutableStatic <- o .: "mutable-static"

        return AppSettings {..}

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
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
