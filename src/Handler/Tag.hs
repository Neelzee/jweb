module Handler.Tag where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist (delete, deleteWhere, get, getBy, insertUnique, update, (=.), (==.))
import Database.Persist.Sql (fromSqlKey)
import Foundation
import Model
import Network.HTTP.Types (ok200)
import Yesod

-- Inline new-tag form (loaded into #tag-creator on the post form)

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

-- Tag management page

getTagListR :: Handler Html
getTagListR = do
  _ <- requireLogin
  tags  <- runDB $ selectList [] [Asc PostTagTag]
  links <- runDB $ selectList [] []
  let countMap :: Map PostTagId Int
      countMap = foldr (\(Entity _ l) m -> Map.insertWith (+) (postTagLinkTagId l) 1 m) Map.empty links
  defaultLayout $ do
    setTitle "Kategorier"
    [whamlet|
      <h1>Kategorier
      <ul>
        $forall Entity tid tag <- tags
          <li id="tag-#{fromSqlKey tid}">
            #{postTagTag tag}
            <small> (#{Map.findWithDefault 0 tid countMap} innlegg)
            <button type="button"
              hx-get=@{TagEditR tid}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="outerHTML">Rediger
            <button
              hx-post=@{TagDeleteR tid}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="delete"
              hx-confirm="Slette «#{postTagTag tag}»? Dette fjerner taggen fra alle innlegg.">
              Slett
    |]

-- Inline edit form for a single row

getTagEditR :: PostTagId -> Handler Html
getTagEditR tid = do
  _ <- requireLogin
  tag <- runDB (get tid) >>= maybe notFound pure
  renderFragment
    [hamlet|
      <li id="tag-#{fromSqlKey tid}">
        <form hx-post=@{TagEditR tid}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="outerHTML">
          <input type="text" name="tag" value="#{postTagTag tag}" required>
          <button type="submit">Lagre
          <button type="button"
            hx-get=@{TagRowR tid}
            hx-target="#tag-#{fromSqlKey tid}"
            hx-swap="outerHTML">Avbryt
    |]

postTagEditR :: PostTagId -> Handler Html
postTagEditR tid = do
  _ <- requireLogin
  newName <- runInputPost $ ireq textField "tag"
  let trimmed = T.strip newName
  mExisting <- runDB $ getBy (UniqueTag trimmed)
  case mExisting of
    Just (Entity existingId _) | existingId /= tid ->
      invalidArgs ["En kategori med dette navnet finnes allerede"]
    _ -> do
      runDB $ update tid [PostTagTag =. trimmed]
      count <- runDB $ length <$> selectList [PostTagLinkTagId ==. tid] []
      renderTagRow tid trimmed count

-- Restores a single row (used by the cancel button)

getTagRowR :: PostTagId -> Handler Html
getTagRowR tid = do
  _ <- requireLogin
  tag   <- runDB (get tid) >>= maybe notFound pure
  count <- runDB $ length <$> selectList [PostTagLinkTagId ==. tid] []
  renderTagRow tid (postTagTag tag) count

postTagDeleteR :: PostTagId -> Handler Html
postTagDeleteR tid = do
  _ <- requireLogin
  runDB $ do
    deleteWhere [PostTagLinkTagId ==. tid]
    delete tid
  sendResponseStatus ok200 ("" :: Text)

-- Shared row fragment

renderTagRow :: PostTagId -> Text -> Int -> Handler Html
renderTagRow tid name count =
  renderFragment
    [hamlet|
      <li id="tag-#{fromSqlKey tid}">
        #{name}
        <small> (#{count} innlegg)
        <button type="button"
          hx-get=@{TagEditR tid}
          hx-target="#tag-#{fromSqlKey tid}"
          hx-swap="outerHTML">Rediger
        <button
          hx-post=@{TagDeleteR tid}
          hx-target="#tag-#{fromSqlKey tid}"
          hx-swap="delete"
          hx-confirm="Slette «#{name}»? Dette fjerner taggen fra alle innlegg.">
          Slett
    |]
