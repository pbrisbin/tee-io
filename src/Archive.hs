module Archive
    ( archivedOutput
    , deleteArchivedOutput
    ) where

import Import

import Control.Lens
import Data.Conduit.Binary (sinkLbs)
import Network.AWS
import Network.AWS.S3

import qualified Data.ByteString.Lazy as BL

archivedOutput :: Token -> Handler BL.ByteString
archivedOutput token = do
    app <- getYesod

    let e = appAWSEnv app
        b = appS3Bucket $ appSettings app
        k = ObjectKey $ tokenText token

    runResourceT $ runAWS e $ do
        rs <- send $ getObject b k
        view gorsBody rs `sinkBody` sinkLbs

deleteArchivedOutput :: Token -> Handler ()
deleteArchivedOutput token = do
    app <- getYesod

    let e = appAWSEnv app
        b = appS3Bucket $ appSettings app
        k = ObjectKey $ tokenText token

    void $ runResourceT $ runAWS e $ send $ deleteObject b k
