module Model where

import ClassyPrelude.Yesod
import Data.Aeson
import Data.UUID
import System.Random

newtype Token = Token { tokenUUID :: UUID }
    deriving (Eq, Random, Read, Show)

instance ToJSON Token where
    toJSON = toJSON . toText . tokenUUID

instance FromJSON Token where
    parseJSON = maybe mzero (return . Token) . fromText <=< parseJSON

tokenText :: Token -> Text
tokenText = toText . tokenUUID

instance PathPiece Token where
    toPathPiece = toText . tokenUUID
    fromPathPiece = fmap Token . fromText

data Command = Command
    { commandRunning :: Bool
    , commandDescription :: Maybe Text
    , commandArchived :: Bool
    , commandCreatedAt :: UTCTime
    , commandUpdatedAt :: UTCTime
    }

instance ToJSON Command where
    toJSON Command{..} = object
        [ "running" .= commandRunning
        , "description" .= commandDescription
        , "archived" .= commandArchived
        , "created_at" .= commandCreatedAt
        , "updated_at" .= commandUpdatedAt
        ]

instance FromJSON Command where
    parseJSON = withObject "Command" $ \o -> Command
        <$> o .: "running"
        <*> o .: "description"
        <*> o .:? "archived" .!= False
        <*> o .: "created_at"
        <*> o .: "updated_at"

data Output = Output
    { outputContent :: Text
    , outputCreatedAt :: UTCTime
    }

instance ToJSON Output where
    toJSON Output{..} = object
        [ "content" .= outputContent
        , "created_at" .= outputCreatedAt
        ]

instance FromJSON Output where
    parseJSON = withObject "Output" $ \o -> Output
        <$> o .: "content"
        <*> o .: "created_at"
