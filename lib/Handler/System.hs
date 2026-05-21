{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.System where

import           Import


-- | App version, start time, and uptime
--
-- operationId: getVersionR
getVersionR :: Handler Value
getVersionR = notImplemented
