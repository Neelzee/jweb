module Handler.Version where

import Data.Int (Int64)
import Data.Time (diffUTCTime, getCurrentTime)
import Foundation
import Yesod

getVersionR :: Handler Value
getVersionR = do
  app <- getYesod
  now <- liftIO getCurrentTime
  let uptime = round (diffUTCTime now (appStartedAt app)) :: Int64
  returnJson $ object
    [ "version"       .= appVersion app
    , "startedAt"     .= appStartedAt app
    , "uptimeSeconds" .= uptime
    ]
