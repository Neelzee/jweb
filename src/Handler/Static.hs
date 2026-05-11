module Handler.Static where

import Control.Monad (unless, when)
import Data.Text (Text)
import qualified Data.Text as T
import Foundation
import System.Directory (doesFileExist)
import System.FilePath (takeExtension, (</>))
import Yesod

getStaticR :: Text -> Handler TypedContent
getStaticR filename = do
  when (T.any (== '/') filename || T.isInfixOf ".." filename) notFound
  app <- getYesod
  let path = appStaticDir app </> T.unpack filename
  exists' <- liftIO $ doesFileExist path
  unless exists' notFound
  sendFile (mimeFor path) path

mimeFor :: FilePath -> ContentType
mimeFor path = case takeExtension path of
  ".css"  -> "text/css"
  ".js"   -> "application/javascript"
  ".png"  -> "image/png"
  ".jpg"  -> "image/jpeg"
  ".jpeg" -> "image/jpeg"
  ".svg"  -> "image/svg+xml"
  ".ico"  -> "image/x-icon"
  _       -> "application/octet-stream"
