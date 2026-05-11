module Application where

import Foundation
import Handler.Auth
import Handler.Home
import Handler.Post
import Yesod (mkYesodDispatch)

mkYesodDispatch "App" resourcesApp
