module Archive
    ( archiveOutput
    , archivedOutput
    , deleteArchivedOutput
    ) where

import Import

import Control.Lens
import Data.Conduit.Binary (sinkLbs)
import Network.AWS
import Network.AWS.S3

import qualified Data.ByteString.Lazy as BL

archiveOutput :: Token -> [Output] -> Handler ()
archiveOutput token outputs = runS3 token $ \b k ->
    void $ send $ putObject b k $ toBody $
        BL.fromStrict $ concatMap (encodeUtf8 . outputContent) outputs

archivedOutput :: Token -> Handler BL.ByteString
archivedOutput token = runS3 token $ \b k -> do
    rs <- send $ getObject b k
    view gorsBody rs `sinkBody` sinkLbs

deleteArchivedOutput :: Token -> Handler ()
deleteArchivedOutput token = runS3 token $ \b k ->
    void $ send $ deleteObject b k

runS3 :: Token -> (BucketName -> ObjectKey -> AWS a) -> Handler a
runS3 token f = do
    app <- getYesod

    let e = appAWSEnv app
        b = appS3Bucket $ appSettings app
        k = ObjectKey $ tokenText token

    runResourceT $ runAWS e $ f b k
