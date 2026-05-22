module Handler.Auth where

import Control.Monad (unless, when)
import System.Environment (lookupEnv)
import qualified Crypto.BCrypt as BCrypt
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Foundation
import Model
import Network.HTTP.Types (internalServerError500, ok200)
import qualified Whitelist
import Yesod

getAuthLoginR :: Handler Html
getAuthLoginR = do
  mUserId <- lookupSession "userId"
  case mUserId of
    Just _ -> redirect JwebHomeR
    Nothing -> defaultLayout $ do
      setTitle "Login"
      [whamlet|
        <div id="login-area" class="max-w-sm mx-auto mt-20 border rounded-2xl p-9 shadow-md">
          <form class="flex flex-col gap-4" hx-post=@{AuthLoginR} hx-target="#login-area" hx-swap="outerHTML">
            <label class="block text-sm font-semibold mb-1.5">Email
            <input type="email" name="email" placeholder="your@email.com" required autofocus class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
            <button type="submit" class="px-5 py-2 rounded-lg text-base font-semibold font-[inherit] border-0 cursor-pointer shadow-sm transition">Fortsett
      |]

postAuthLoginR :: Handler Html
postAuthLoginR = do
  email <- runInputPost $ ireq textField "email"
  mStep <- runInputPost $ iopt textField "step"
  case mStep of
    Nothing -> handleEmailStep email
    Just "login" -> handlePasswordStep email
    Just "setup" -> handleSetupStep email
    Just _ -> invalidArgs ["Unknown step"]

handleEmailStep :: Text -> Handler Html
handleEmailStep email = do
  unless (Whitelist.member email) $
    sendFragment
      [hamlet|
      <div id="login-area" class="max-w-sm mx-auto mt-20 border rounded-2xl p-9 shadow-md">
        <p class="error text-sm">Not authorized.
    |]
  mUser <- runDB $ getBy (UniqueEmail email)
  case mUser of
    Just (Entity _ user)
      | isJust (userPasswordHash user) ->
          sendFragment
            [hamlet|
        <div id="login-area" class="max-w-sm mx-auto mt-20 border rounded-2xl p-9 shadow-md">
          <form class="flex flex-col gap-4" hx-post=@{AuthLoginR} hx-target="#login-area" hx-swap="outerHTML">
            <input type="hidden" name="email" value="#{email}">
            <input type="hidden" name="step" value="login">
            <p class="text-sm mb-3.5">Velkommen, #{userName user}
            <label class="block text-sm font-semibold mb-1.5">Passord
            <input type="password" name="password" placeholder="Passord" required autofocus class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
            <button type="submit" class="px-5 py-2 rounded-lg text-base font-semibold font-[inherit] border-0 cursor-pointer shadow-sm transition">Logg inn
      |]
    _ ->
      sendFragment
        [hamlet|
        <div id="login-area" class="max-w-sm mx-auto mt-20 border rounded-2xl p-9 shadow-md">
          <form class="flex flex-col gap-4" hx-post=@{AuthLoginR} hx-target="#login-area" hx-swap="outerHTML">
            <input type="hidden" name="email" value="#{email}">
            <input type="hidden" name="step" value="setup">
            <p class="text-sm mb-3.5">Sett eit passord for #{email}
            <label class="block text-sm font-semibold mb-1.5">Passord
            <input type="password" name="password" placeholder="Password" required autofocus class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
            <label class="block text-sm font-semibold mb-1.5">Bekreft
            <input type="password" name="confirm" placeholder="Confirm password" required class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
            <button type="submit" class="px-5 py-2 rounded-lg text-base font-semibold font-[inherit] border-0 cursor-pointer shadow-sm transition">Sett passord
      |]

handlePasswordStep :: Text -> Handler Html
handlePasswordStep email = do
  unless (Whitelist.member email) $ sendFragment errNotAuthorized
  password <- runInputPost $ ireq textField "password"
  mUser <- runDB $ getBy (UniqueEmail email)
  case mUser of
    Nothing -> sendFragment errNotAuthorized
    Just (Entity uid user) ->
      case userPasswordHash user of
        Nothing -> sendFragment errNotAuthorized
        Just hash ->
          if checkPwd hash password
            then do
              setSession "userId" (toPathPiece uid)
              addHeader "HX-Redirect" "/"
              sendResponseStatus ok200 ("" :: Text)
            else
              sendFragment
                [hamlet|
              <div id="login-area" class="max-w-sm mx-auto mt-20 border rounded-2xl p-9 shadow-md">
                <form class="flex flex-col gap-4" hx-post=@{AuthLoginR} hx-target="#login-area" hx-swap="outerHTML">
                  <input type="hidden" name="email" value="#{email}">
                  <input type="hidden" name="step" value="login">
                  <p class="error text-sm mb-3.5">Feil passord.
                  <label class="block text-sm font-semibold mb-1.5">Passord
                  <input type="password" name="password" required autofocus class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
                  <button type="submit" class="px-5 py-2 rounded-lg text-base font-semibold font-[inherit] border-0 cursor-pointer shadow-sm transition">Logg inn
            |]

handleSetupStep :: Text -> Handler Html
handleSetupStep email = do
  unless (Whitelist.member email) $ sendFragment errNotAuthorized
  password <- runInputPost $ ireq textField "password"
  confirm <- runInputPost $ ireq textField "confirm"
  when (password /= confirm) $
    sendFragment
      [hamlet|
      <div id="login-area" class="max-w-sm mx-auto mt-20 border rounded-2xl p-9 shadow-md">
        <form class="flex flex-col gap-4" hx-post=@{AuthLoginR} hx-target="#login-area" hx-swap="outerHTML">
          <input type="hidden" name="email" value="#{email}">
          <input type="hidden" name="step" value="setup">
          <p class="error text-sm mb-3.5">Passorda er ikkje like.
          <label class="block text-sm font-semibold mb-1.5">Passord
          <input type="password" name="password" required autofocus class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
          <label class="block text-sm font-semibold mb-1.5">Bekreft
          <input type="password" name="confirm" required class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
          <button type="submit" class="px-5 py-2 rounded-lg text-base font-semibold font-[inherit] border-0 cursor-pointer shadow-sm transition">Sett passord
    |]
  mHash <- liftIO $ hashPwd password
  hash <- maybe errInternal pure mHash
  let displayName = maybe email id (Whitelist.lookupName email)
  mUser <- runDB $ getBy (UniqueEmail email)
  uid <- case mUser of
    Nothing -> runDB $ insert (User email displayName (Just hash))
    Just (Entity uid _) -> do
      runDB $ update uid [UserPasswordHash =. Just hash]
      pure uid
  setSession "userId" (toPathPiece uid)
  addHeader "HX-Redirect" "/"
  sendResponseStatus ok200 ("" :: Text)

postAuthLogoutR :: Handler ()
postAuthLogoutR = do
  deleteSession "userId"
  redirect JwebHomeR

getAuthTestSessionR :: Handler Html
getAuthTestSessionR = do
  mTestMode <- liftIO $ lookupEnv "JWEB_TEST_MODE"
  case mTestMode of
    Nothing -> notFound
    Just _ -> do
      mUser <- runDB $ selectFirst ([] :: [Filter User]) []
      case mUser of
        Nothing -> notFound
        Just (Entity uid _) -> do
          setSession "userId" (toPathPiece uid)
          redirect JwebHomeR

-- Helpers

sendFragment :: HtmlUrl (Route App) -> Handler a
sendFragment f = do
  html <- renderFragment f
  sendResponse html

hashPwd :: Text -> IO (Maybe Text)
hashPwd pwd =
  fmap decodeUtf8
    <$> BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy (encodeUtf8 pwd)

checkPwd :: Text -> Text -> Bool
checkPwd hash pwd = BCrypt.validatePassword (encodeUtf8 hash) (encodeUtf8 pwd)

errNotAuthorized :: HtmlUrl (Route App)
errNotAuthorized =
  [hamlet|
    <div id="login-area" class="max-w-sm mx-auto mt-20 border rounded-2xl p-9 shadow-md">
      <p class="error text-sm">Not authorized.
  |]

errInternal :: Handler a
errInternal = sendResponseStatus internalServerError500 ("Internal error" :: Text)
