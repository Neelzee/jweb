module Handler.Tag where

import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (getBy, insertUnique)
import Database.Persist.Sql (fromSqlKey)
import Foundation
import Model
import Yesod

getTagNewR :: Handler Html
getTagNewR = do
  _ <- requireLogin
  renderFragment
    [hamlet|
      <form hx-post=@{TagCreateR} hx-target="#tag-creator" hx-swap="innerHTML">
        <input type="text" name="tag" required placeholder="Ny kategori">
        <button type="submit">Opprett
    |]

postTagCreateR :: Handler Html
postTagCreateR = do
  _ <- requireLogin
  tagName <- runInputPost $ ireq textField "tag"
  let trimmed = T.strip tagName
  mTid <- runDB $ do
    mNew <- insertUnique (PostTag trimmed)
    case mNew of
      Just tid -> pure (Just tid)
      Nothing  -> fmap entityKey <$> getBy (UniqueTag trimmed)
  case mTid of
    Nothing  -> invalidArgs ["Tag kunne ikke opprettes"]
    Just tid ->
      renderFragment
        [hamlet|
          <button type="button" hx-get=@{TagNewR} hx-target="#tag-creator" hx-swap="innerHTML">
            + Ny kategori
          <select hx-swap-oob="beforeend:#tags-select" style="display:none">
            <option value="#{fromSqlKey tid}" selected>#{trimmed}
        |]
