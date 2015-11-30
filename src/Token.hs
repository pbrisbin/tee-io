module Token
    ( Token(..)
    , newToken
    , tokenText
    ) where

import ClassyPrelude.Yesod
import Data.UUID
import Database.Persist.Sql
import System.Random

newtype Token = Token { tokenUUID :: UUID }
    deriving (Eq, Random, Read, Show)

newToken :: MonadIO m => m Token
newToken = liftIO $ randomIO

tokenText :: Token -> Text
tokenText = toText . tokenUUID

fromTextEither :: Text -> Either Text Token
fromTextEither x = case fromText x of
    Just y -> Right $ Token y
    Nothing -> Left $ "Invalid UUID: " <> x

instance ToJSON Token where
    toJSON = toJSON . tokenText

instance FromJSON Token where
    parseJSON = maybe mzero (return . Token) . fromText <=< parseJSON

instance PathPiece Token where
    toPathPiece = tokenText
    fromPathPiece = fmap Token . fromText

instance PersistField Token where
    toPersistValue = toPersistValue . tokenText
    fromPersistValue = fromTextEither <=< fromPersistValue

instance PersistFieldSql Token where
    sqlType _ = SqlString
