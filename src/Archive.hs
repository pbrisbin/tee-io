module Archive
    ( putOutput
    , getOutput
    , deleteOutput
    ) where

import Import

import qualified Aws as Aws
import qualified Aws.S3 as S3

putOutput :: Token -> [Output] -> Handler ()
putOutput token outputs = do
    bucket <- appS3Bucket . appSettings <$> getYesod

    let bytestrings = map (encodeUtf8 . outputContent) outputs
        contentLength = fromIntegral $ sum $ map length bytestrings

    $(logDebug) $ "archiving output to S3 " <> tokenText token

    runS3 (S3.putObject bucket (tokenText token)
        $ requestBodySource contentLength $ yieldMany bytestrings)
        $ ignore

getOutput :: Token -> Sink ByteString (ResourceT IO) a -> Handler a
getOutput token sink = do
    bucket <- appS3Bucket . appSettings <$> getYesod

    $(logDebug) $ "retrieving archived output " <> tokenText token

    runS3 (S3.getObject bucket $ tokenText token)
        $ \rsp -> responseBody (S3.gorResponse rsp) $$+- sink

deleteOutput :: Token -> Handler ()
deleteOutput token = do
    bucket <- appS3Bucket . appSettings <$> getYesod

    $(logDebug) $ "deleting archived output " <> tokenText token

    runS3 (S3.deleteObjects bucket [tokenText token]) $ ignore

runS3
    :: (Aws.Transaction r a, Aws.ServiceConfiguration r ~ S3.S3Configuration)
    => r -> (a -> ResourceT IO b) -> Handler b
runS3 a f = do
    app <- getYesod

    let cfg = appAWSConfiguration app
        mgr = appHttpManager app
        s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

    liftIO $ runResourceT (f =<< Aws.pureAws cfg s3cfg mgr a)

ignore :: Monad m => a -> m ()
ignore _ = return ()
