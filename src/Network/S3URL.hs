module Network.S3URL
    ( S3URL(..)
    , parseS3URL
    ) where

import Prelude

import Data.Aeson
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Lens (view)
import Network.URI
import Text.Read (readMaybe)
import Network.AWS (Region(..), Service, endpointHost)
import Network.AWS.Endpoint (defaultEndpoint, setEndpoint)
import Network.AWS.S3 (BucketName(..), s3)

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

data S3URL = S3URL
    { s3Service :: Service
    , s3Bucket :: BucketName
    }

instance FromJSON S3URL where
    parseJSON = withText "URL" $
        either (fail . ("Invalid S3 URL: " <>)) return . parseS3URL

parseS3URL :: Text -> Either String S3URL
parseS3URL t = do
    uri <- wrap "invalid URI" $ parseURI $ T.unpack t

    let auth = fromMaybe (URIAuth "" "" "") $ uriAuthority uri
        host = C8.pack $ if null $ uriRegName auth
            then defaultS3Host
            else uriRegName auth

    port <- case uriPort auth of
        (':':x) -> wrap "invalid port" $ readMaybe x
        _ -> wrap "cannot infer port" $ portForScheme $ uriScheme uri

    bucket <- wrap "bucket not provided" $ case uriPath uri of
        ('/':x:xs) -> Just $ x:xs
        _ -> Nothing

    return S3URL
        { s3Service = setEndpoint (uriScheme uri == "https:") host port s3
        , s3Bucket = BucketName $ T.pack bucket
        }

  where
    wrap msg = maybe (Left msg) Right

    defaultS3Host = C8.unpack
        $ view endpointHost
        $ defaultEndpoint s3 NorthVirginia

    portForScheme "http:" = Just 80
    portForScheme "https:" = Just 443
    portForScheme _ = Nothing
