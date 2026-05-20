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
      <form class="flex items-center gap-2" hx-post=@{TagCreateR} hx-target="#tag-creator" hx-swap="innerHTML">
        <input type="text" name="tag" required placeholder="Ny kategori" class="px-2 py-1 border rounded text-sm font-[inherit]">
        <button type="submit" class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]">Opprett
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
          <button type="button" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer" hx-get=@{TagNewR} hx-target="#tag-creator" hx-swap="innerHTML">
            + Ny kategori
          <select hx-swap-oob="beforeend:#tags-select" style="display:none">
            <option value="#{fromSqlKey tid}" selected>#{trimmed}
        |]

-- Inline tag management fragment (loaded into #tags-area from the post form)

getTagInlineR :: Handler Html
getTagInlineR = do
  _ <- requireLogin
  tags  <- runDB $ selectList [] [Asc PostTagTag]
  links <- runDB $ selectList [] []
  let countMap :: Map PostTagId Int
      countMap = foldr (\(Entity _ l) m -> Map.insertWith (+) (postTagLinkTagId l) 1 m) Map.empty links
  renderFragment
    [hamlet|
      <ul class="flex flex-col gap-2 list-none">
        $forall Entity tid tag <- tags
          <li id="tag-#{fromSqlKey tid}" class="flex items-center gap-3 text-sm">
            #{postTagTag tag}
            <small>(#{Map.findWithDefault 0 tid countMap} innlegg)
            <button type="button" class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]"
              hx-get=@{TagEditR tid}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="outerHTML">Rediger
            <button class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]"
              hx-post=@{TagDeleteR tid}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="delete"
              hx-confirm="Slette «#{postTagTag tag}»? Dette fjerner taggen fra alle innlegg.">
              Slett
      <button type="button" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer" hx-get=@{TagSelectR} hx-target="#tags-area" hx-swap="innerHTML">
        Tilbake
    |]

-- Restores the tag selector (used by the Tilbake button in the inline manager)

getTagSelectR :: Handler Html
getTagSelectR = do
  _ <- requireLogin
  allTags <- runDB $ selectList [] [Asc PostTagTag]
  renderFragment
    [hamlet|
      <label class="block text-sm font-semibold mb-1.5">Kategori
      <select id="tags-select" name="tags" multiple class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
        $forall Entity tid tag <- allTags
          <option value="#{fromSqlKey tid}">#{postTagTag tag}
      <div id="tag-creator" class="flex items-center gap-2">
        <button type="button" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer" hx-get=@{TagNewR} hx-target="#tag-creator" hx-swap="innerHTML">
          + Ny kategori
      <button type="button" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer" hx-get=@{TagInlineR} hx-target="#tags-area" hx-swap="innerHTML">
        Rediger kategorier
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
      <h1 class="text-xl font-bold tracking-tight mb-6">Kategorier
      <ul class="flex flex-col gap-2 list-none">
        $forall Entity tid tag <- tags
          <li id="tag-#{fromSqlKey tid}" class="flex items-center gap-3 text-sm">
            #{postTagTag tag}
            <small>(#{Map.findWithDefault 0 tid countMap} innlegg)
            <button type="button" class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]"
              hx-get=@{TagEditR tid}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="outerHTML">Rediger
            <button class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]"
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
      <li id="tag-#{fromSqlKey tid}" class="flex items-center gap-3 text-sm">
        <form class="flex items-center gap-2" hx-post=@{TagEditR tid}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="outerHTML">
          <input type="text" name="tag" value="#{postTagTag tag}" required class="px-2 py-1 border rounded text-sm font-[inherit]">
          <button type="submit" class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]">Lagre
          <button type="button" class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]"
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
      <li id="tag-#{fromSqlKey tid}" class="flex items-center gap-3 text-sm">
        #{name}
        <small>(#{count} innlegg)
        <button type="button" class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]"
          hx-get=@{TagEditR tid}
          hx-target="#tag-#{fromSqlKey tid}"
          hx-swap="outerHTML">Rediger
        <button class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]"
          hx-post=@{TagDeleteR tid}
          hx-target="#tag-#{fromSqlKey tid}"
          hx-swap="delete"
          hx-confirm="Slette «#{name}»? Dette fjerner taggen fra alle innlegg.">
          Slett
    |]
