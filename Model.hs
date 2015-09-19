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

newtype OutputToken = OutputToken Token
    deriving (Show, FromJSON, ToJSON)

instance PathPiece Token where
    toPathPiece = toText . tokenUUID
    fromPathPiece = fmap Token . fromText

data Command = Command
    { commandRunning :: Bool
    , commandDescription :: Text
    }

instance ToJSON Command where
    toJSON Command{..} = object
        [ "running" .= commandRunning
        , "description" .= commandDescription
        ]

instance FromJSON Command where
    parseJSON = withObject "Command" $ \o -> Command
        <$> o .:? "running" .!= True
        <*> o .:? "description" .!= ""

data CommandData = CommandData
    { cdCommand :: Command
    , cdOutputToken :: OutputToken
    }

instance ToJSON CommandData where
    toJSON CommandData{..} = object
        [ "command" .= cdCommand
        , "output_token" .= cdOutputToken
        ]

instance FromJSON CommandData where
    parseJSON = withObject "CommandData" $ \o -> CommandData
        <$> o .: "command"
        <*> o .: "output_token"

data Output = Output
    { outputContent :: Text
    }

instance ToJSON Output where
    toJSON Output{..} = object
        [ "content" .= outputContent
        ]

instance FromJSON Output where
    parseJSON = withObject "Output" $ \o -> Output
        <$> o .: "content"
