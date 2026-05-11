module Foundation where

import Data.Maybe (isJust)
import Data.Text (Text)
import Database.Persist.Sqlite (ConnectionPool, SqlBackend, runSqlPool)
import Model
import Yesod

data App = App
  { appConnectionPool :: ConnectionPool
  , appUploadDir      :: FilePath
  , appSessionKeyPath :: FilePath
  , appStaticDir      :: FilePath
  }

mkYesodData "App" [parseRoutes|
/                     HomeR        GET
/auth/login           AuthLoginR   GET POST
/auth/logout          AuthLogoutR  POST
/post/new             PostNewR     GET POST
/post/#PostId/edit    PostEditR    GET POST
/post/#PostId/delete  PostDeleteR  POST
/uploads/#Text        UploadsR     GET
/static/#Text         StaticR      GET
/trash                TrashR       GET
/post/#PostId/restore PostRestoreR POST
|]

instance Yesod App where
  makeSessionBackend app = Just <$>
    defaultClientSessionBackend (30 * 24 * 60) (appSessionKeyPath app)

  defaultLayout widget = do
    pc      <- widgetToPageContent widget
    req     <- getRequest
    mUserId <- lookupSession "userId"
    let loggedIn = isJust mUserId
        mToken   = reqToken req
    withUrlRenderer [hamlet|
      $doctype 5
      <html lang="en">
        <head>
          <meta charset="UTF-8">
          <meta name="viewport" content="width=device-width, initial-scale=1">
          $maybe token <- mToken
            <meta name="csrf-token" content="#{token}">
          <title>#{pageTitle pc}
          <link rel="stylesheet" href=@{StaticR "style.css"}>
          <script src="https://unpkg.com/htmx.org@2.0.4" defer>
          <script>
            document.addEventListener('DOMContentLoaded', function () {
              document.addEventListener('htmx:configRequest', function (e) {
                var m = document.querySelector('meta[name="csrf-token"]');
                if (m) e.detail.headers['X-CSRF-Token'] = m.getAttribute('content');
              });
            });
          ^{pageHead pc}
        <body>
          <header>
            <a href=@{HomeR}>Ønskeliste</a>
            $if loggedIn
              <a href=@{TrashR}>Papirkurv
              <form method="post" action=@{AuthLogoutR}>
                $maybe token <- mToken
                  <input type="hidden" name="_token" value="#{token}">
                <button type="submit">Logg ut
          <main>
            ^{pageBody pc}
    |]

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    app <- getYesod
    runSqlPool action (appConnectionPool app)

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

renderFragment :: HtmlUrl (Route App) -> Handler Html
renderFragment f = f <$> getUrlRenderParams

requireLogin :: Handler UserId
requireLogin = do
  mText <- lookupSession "userId"
  case mText >>= fromPathPiece of
    Just uid -> pure uid
    Nothing  -> redirect AuthLoginR
