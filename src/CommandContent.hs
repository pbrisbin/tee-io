module CommandContent
    ( CommandContent(..)
    , findContent404
    , contentHeader
    , contentBody
    ) where

import Import
import Archive

-- import Network.AWS
-- import Network.AWS.S3 (_NoSuchKey)

import qualified Data.ByteString.Lazy as BL

data CommandContent
    = Live Token Command
    | Archived BL.ByteString

findContent404 :: Token -> YesodDB App CommandContent
findContent404 token = do
    mcommand <- getBy $ UniqueCommand token

    case mcommand of
        Just (Entity _ command) -> return $ Live token command
        _ -> lift $ Archived <$> archivedOutput token

        -- Need to deal with MonadCatch
        -- _ -> lift $ Archived
        --     <$> catching _NoSuchKey (archivedOutput token) (\_ -> notFound)

contentHeader :: CommandContent -> Widget
contentHeader (Live _ command) = [whamlet|
    <span .right>
        #{show $ commandCreatedAt command}

    $maybe desc <- commandDescription command
        #{desc}
    $nothing
        No description
|]
contentHeader (Archived _) = [whamlet|
    Archived output
|]


contentBody :: CommandContent -> Widget
contentBody (Live token _) = [whamlet|
    <code #output data-stream-url=@{OutputR token}>
|]
contentBody (Archived content) = [whamlet|
    <code>#{decodeUtf8 content}
|]
