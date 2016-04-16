module Settings.StaticFiles where

import Settings (appStaticDir, compileTimeAppSettings)

import Yesod.Static (staticFiles)

staticFiles (appStaticDir compileTimeAppSettings)
