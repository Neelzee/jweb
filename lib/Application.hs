{-# OPTIONS_GHC -Wno-orphans #-}

module Application where

import Foundation
import Handler.Auth
import Handler.Docs
import Handler.Home
import Handler.Post
import Handler.Static
import Handler.Tag
import Handler.Trash
import Handler.Version
import Yesod (mkYesodDispatch)

mkYesodDispatch "App" resourcesApp
