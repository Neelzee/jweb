module Handler.Docs where

import Foundation
import Yesod

getDocsR :: Handler Html
getDocsR = defaultLayout $ do
  setTitle "API Docs"
  [whamlet|
    <redoc spec-url="/static/specification.yaml">
    <script src="https://cdn.jsdelivr.net/npm/redoc/bundles/redoc.standalone.js">
  |]
