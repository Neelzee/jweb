module Foundation where

import Data.Maybe (isJust)
import Data.Text (Text)
import Database.Persist.Sqlite (ConnectionPool, SqlBackend, runSqlPool)
import Model
import Yesod

data App = App
  { appConnectionPool :: ConnectionPool,
    appUploadDir :: FilePath,
    appSessionKeyPath :: FilePath,
    appStaticDir :: FilePath
  }

mkYesodData
  "App"
  [parseRoutes|
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
/tag/new               TagNewR     GET
/tag                   TagCreateR  POST
/tags                  TagListR    GET
/tags/inline           TagInlineR  GET
/tags/select           TagSelectR  GET
/tag/#PostTagId/edit   TagEditR    GET POST
/tag/#PostTagId/delete TagDeleteR  POST
/tag/#PostTagId/row    TagRowR     GET
|]

instance Yesod App where
  makeSessionBackend app =
    Just
      <$> defaultClientSessionBackend (30 * 24 * 60) (appSessionKeyPath app)

  defaultLayout widget = do
    pc <- widgetToPageContent widget
    req <- getRequest
    mUserId <- lookupSession "userId"
    let loggedIn = isJust mUserId
        mToken = reqToken req
    withUrlRenderer
      [hamlet|
      $doctype 5
      <html lang="en">
        <head>
          <meta charset="UTF-8">
          <meta name="viewport" content="width=device-width, initial-scale=1">
          $maybe token <- mToken
            <meta name="csrf-token" content="#{token}">
          <title>#{pageTitle pc}
          <link rel="stylesheet" href=@{StaticR "output.css"}>
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
          <header class="sticky top-0 z-10 flex items-center justify-between px-8 h-14 border-b shadow-sm">
            <a href=@{HomeR} class="text-base font-bold no-underline tracking-tight">Ønskeliste</a>
            $if loggedIn
              <form method="post" action=@{AuthLogoutR} class="flex items-center">
                $maybe token <- mToken
                  <input type="hidden" name="_token" value="#{token}">
                <button type="submit" class="rounded border px-3.5 py-1.5 text-sm font-medium font-[inherit] cursor-pointer transition-colors">Logg ut
          <main class="max-w-[52rem] mx-auto mt-10 px-6">
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
    Nothing -> redirect AuthLoginR
    Just uid -> do
      mUser <- runDB $ get uid
      case mUser of
        Just _ -> pure uid
        Nothing -> deleteSession "userId" >> redirect AuthLoginR
