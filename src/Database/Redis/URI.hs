module Database.Redis.URI
    ( connectURI
    , parseURI
    ) where

import Prelude

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Redis
    ( ConnectInfo(..)
    , Connection
    , HostName
    , PortID(..)
    , connect
    , defaultConnectInfo
    )
import Text.Parsec
import Text.Parsec.Text

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

connectURI :: Text -> IO Connection
connectURI uri = maybe err connect $ parseURI uri
  where
    err = error $ "invalid URI: " ++ T.unpack uri

parseURI :: Text -> Maybe ConnectInfo
parseURI = either (const Nothing) Just . parse parser ""

parser :: Parser ConnectInfo
parser = do
    void $ string "redis://"
    auth <- parseAuth
    host <- parseHost
    port <- parsePort

    return $ defaultConnectInfo
        { connectHost = host
        , connectPort = port
        , connectAuth = auth
        }

parseAuth :: Parser (Maybe ByteString)
parseAuth = optionMaybe $ try $ do
    void $ anyTill ':' -- user ignored
    fmap C8.pack $ anyTill '@'

parseHost :: Parser HostName
parseHost = anyTill ':'

parsePort :: Parser PortID
parsePort = fmap (PortNumber . fromInteger . read) $ many digit

anyTill :: Char -> Parser String
anyTill = manyTill anyToken . try . char
