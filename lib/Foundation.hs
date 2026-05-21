module Foundation where

import Data.Int (Int64)
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

mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

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
          <link rel="stylesheet" href=@{StaticByTextR "output.css"}>
          <script src="https://unpkg.com/htmx.org@2.0.4" defer>
          <script src=@{StaticByTextR "main.js"} defer>
          <script>
            document.addEventListener('DOMContentLoaded', function () {
              document.addEventListener('htmx:configRequest', function (e) {
                var m = document.querySelector('meta[name="csrf-token"]');
                if (m) e.detail.headers['X-CSRF-Token'] = m.getAttribute('content');
              });
            });
          ^{pageHead pc}
        <body>
          <header class="sticky top-0 z-10 flex items-center justify-between px-4 sm:px-8 h-14 border-b shadow-sm bg-white">
            <a href=@{JwebHomeR} class="text-base font-bold no-underline tracking-tight">Ønskeliste</a>
            $if loggedIn
              <form method="post" action=@{AuthLogoutR} class="hidden sm:flex items-center">
                $maybe token <- mToken
                  <input type="hidden" name="_token" value="#{token}">
                <button type="submit" class="rounded border px-3.5 py-1.5 text-sm font-medium font-[inherit] cursor-pointer transition-colors">Logg ut
              <div class="sm:hidden relative" data-menu-wrapper>
                <button type="button" onclick="var m=document.getElementById('mobile-menu');m.classList.toggle('hidden')" class="rounded border px-3 py-1.5 text-sm cursor-pointer font-[inherit]">&#9776;
                <div id="mobile-menu" class="hidden absolute right-0 top-10 border rounded-lg shadow-lg z-50 w-36 bg-white">
                  <a href=@{TrashR} class="block px-4 py-2.5 text-sm font-medium no-underline">Papirkurv
                  <form method="post" action=@{AuthLogoutR}>
                    $maybe token <- mToken
                      <input type="hidden" name="_token" value="#{token}">
                    <button type="submit" class="block w-full px-4 py-2.5 text-sm text-left font-medium font-[inherit] bg-transparent border-0 cursor-pointer">Logg ut
          <main class="max-w-[52rem] mx-auto mt-6 sm:mt-10 px-4 sm:px-6">
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
