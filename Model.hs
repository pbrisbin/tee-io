module Model where

import ClassyPrelude.Yesod
import Data.UUID
import System.Random

newtype Token = Token { tokenUUID :: UUID }
    deriving (Eq, Random, Read, Show)

tokenToBS :: Token -> ByteString
tokenToBS = encodeUtf8 . toText . tokenUUID

instance PathPiece Token where
    toPathPiece = toText . tokenUUID
    fromPathPiece = fmap Token . fromText

data Command = Command
    { commandRunning :: Bool
    }

instance ToJSON Command where
    toJSON Command{..} = object
        [ "running" .= commandRunning
        ]

newCommand :: Command
newCommand = Command
    { commandRunning = False
    }
