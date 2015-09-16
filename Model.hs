module Model where

import ClassyPrelude.Yesod
import Data.UUID
import System.Random

newtype Token = Token { tokenUUID :: UUID }
    deriving (Eq, Random, Read, Show)

instance PathPiece Token where
    toPathPiece = toText . tokenUUID
    fromPathPiece = fmap Token . fromText
