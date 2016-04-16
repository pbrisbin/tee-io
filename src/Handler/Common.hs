module Handler.Common where

import Import

import Data.FileEmbed (embedFile)

getFaviconR :: Handler TypedContent
getFaviconR = return
    $ TypedContent "image/x-icon"
    $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return
    $ TypedContent typePlain
    $ toContent $(embedFile "config/robots.txt")
