{-# OPTIONS_GHC -Wno-orphans #-}

module Application where

import Foundation
import Handler.Auth
import Handler.Home
import Handler.Post
import Handler.Static
import Handler.Tag
import Handler.Trash
import Yesod (mkYesodDispatch)

mkYesodDispatch "App" resourcesApp
