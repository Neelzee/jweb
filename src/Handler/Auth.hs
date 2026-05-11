module Handler.Auth where

import Control.Monad (unless, when)
import qualified Crypto.BCrypt as BCrypt
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.Persist (Entity (..), getBy, insert, update, (=.))
import Foundation
import Model
import Network.HTTP.Types (internalServerError500, ok200)
import qualified Whitelist
import Yesod

getAuthLoginR :: Handler Html
getAuthLoginR = do
  mUserId <- lookupSession "userId"
  case mUserId of
    Just _  -> redirect HomeR
    Nothing -> defaultLayout $ do
      setTitle "Login"
      [whamlet|
        <div #login-area>
          <form hx-post=@{AuthLoginR} hx-target="#login-area" hx-swap="outerHTML">
            <label>Email
            <input type="email" name="email" placeholder="your@email.com" required autofocus>
            <button type="submit">Fortsett
      |]

postAuthLoginR :: Handler Html
postAuthLoginR = do
  email <- runInputPost $ ireq textField "email"
  mStep <- runInputPost $ iopt textField "step"
  case mStep of
    Nothing       -> handleEmailStep email
    Just "login"  -> handlePasswordStep email
    Just "setup"  -> handleSetupStep email
    Just _        -> invalidArgs ["Unknown step"]

handleEmailStep :: Text -> Handler Html
handleEmailStep email = do
  unless (Whitelist.member email) $
    sendFragment [hamlet|
      <div #login-area>
        <p .error>Not authorized.
    |]
  mUser <- runDB $ getBy (UniqueEmail email)
  case mUser of
    Just (Entity _ user) | isJust (userPasswordHash user) ->
      sendFragment [hamlet|
        <div #login-area>
          <form hx-post=@{AuthLoginR} hx-target="#login-area" hx-swap="outerHTML">
            <input type="hidden" name="email" value="#{email}">
            <input type="hidden" name="step" value="login">
            <p>Velkommen, #{userName user}
            <label>Passord
            <input type="password" name="password" placeholder="Password" required autofocus>
            <button type="submit">Log in
      |]
    _ ->
      sendFragment [hamlet|
        <div #login-area>
          <form hx-post=@{AuthLoginR} hx-target="#login-area" hx-swap="outerHTML">
            <input type="hidden" name="email" value="#{email}">
            <input type="hidden" name="step" value="setup">
            <p>Sett eit passord for #{email}
            <label>Passord
            <input type="password" name="password" placeholder="Password" required autofocus>
            <label>Bekreft
            <input type="password" name="confirm" placeholder="Confirm password" required>
            <button type="submit">Sett passord
      |]

handlePasswordStep :: Text -> Handler Html
handlePasswordStep email = do
  unless (Whitelist.member email) $ sendFragment errNotAuthorized
  password <- runInputPost $ ireq textField "password"
  mUser    <- runDB $ getBy (UniqueEmail email)
  case mUser of
    Nothing -> sendFragment errNotAuthorized
    Just (Entity uid user) ->
      case userPasswordHash user of
        Nothing   -> sendFragment errNotAuthorized
        Just hash ->
          if checkPwd hash password
            then do
              setSession "userId" (toPathPiece uid)
              addHeader "HX-Redirect" "/"
              sendResponseStatus ok200 ("" :: Text)
            else sendFragment [hamlet|
              <div #login-area>
                <form hx-post=@{AuthLoginR} hx-target="#login-area" hx-swap="outerHTML">
                  <input type="hidden" name="email" value="#{email}">
                  <input type="hidden" name="step" value="login">
                  <p .error>Feil passord.
                  <label>Passord
                  <input type="password" name="password" required autofocus>
                  <button type="submit">Logg inn
            |]

handleSetupStep :: Text -> Handler Html
handleSetupStep email = do
  unless (Whitelist.member email) $ sendFragment errNotAuthorized
  password <- runInputPost $ ireq textField "password"
  confirm  <- runInputPost $ ireq textField "confirm"
  when (password /= confirm) $
    sendFragment [hamlet|
      <div #login-area>
        <form hx-post=@{AuthLoginR} hx-target="#login-area" hx-swap="outerHTML">
          <input type="hidden" name="email" value="#{email}">
          <input type="hidden" name="step" value="setup">
          <p .error>Passorda er ikkje like.
          <label>Passord
          <input type="password" name="password" required autofocus>
          <label>Bekreft
          <input type="password" name="confirm" required>
          <button type="submit">Sett passord
    |]
  mHash <- liftIO $ hashPwd password
  hash  <- maybe errInternal pure mHash
  let displayName = maybe email id (Whitelist.lookupName email)
  mUser <- runDB $ getBy (UniqueEmail email)
  uid <- case mUser of
    Nothing              -> runDB $ insert (User email displayName (Just hash))
    Just (Entity uid _)  -> do
      runDB $ update uid [UserPasswordHash =. Just hash]
      pure uid
  setSession "userId" (toPathPiece uid)
  addHeader "HX-Redirect" "/"
  sendResponseStatus ok200 ("" :: Text)

postAuthLogoutR :: Handler ()
postAuthLogoutR = do
  deleteSession "userId"
  redirect HomeR

-- Helpers

sendFragment :: HtmlUrl (Route App) -> Handler a
sendFragment f = do
  html <- renderFragment f
  sendResponse html

hashPwd :: Text -> IO (Maybe Text)
hashPwd pwd = fmap decodeUtf8 <$>
  BCrypt.hashPasswordUsingPolicy BCrypt.slowerBcryptHashingPolicy (encodeUtf8 pwd)

checkPwd :: Text -> Text -> Bool
checkPwd hash pwd = BCrypt.validatePassword (encodeUtf8 hash) (encodeUtf8 pwd)

errNotAuthorized :: HtmlUrl (Route App)
errNotAuthorized = [hamlet|<div #login-area><p .error>Not authorized.|]

errInternal :: Handler a
errInternal = sendResponseStatus internalServerError500 ("Internal error" :: Text)
