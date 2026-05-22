module Handler.Tag where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Text.Read (readMaybe)
import Foundation
import Model
import Network.HTTP.Types (ok200)
import Yesod

getTagNewR :: Handler Html
getTagNewR = do
  _ <- requireLogin
  renderFragment
    [hamlet|
      <form class="flex items-center gap-2" hx-post=@{TagR} hx-target="#tag-creator" hx-swap="innerHTML" hx-include="#selected-tags-holder input">
        <input type="text" name="tag" required placeholder="Ny kategori" class="px-2 py-1 border rounded text-sm font-[inherit]">
        <button type="submit" class="px-2 py-1 text-sm rounded cursor-pointer font-[inherit] bg-green-600 text-white">Opprett
    |]

postTagR :: Handler Html
postTagR = do
  _ <- requireLogin
  tagName <- runInputPost $ ireq textField "tag"
  let trimmed = T.strip tagName
  mNew <- runDB $ insertUnique (PostTag trimmed)
  case mNew of
    Nothing ->
      renderFragment
        [hamlet|
          <button type="button" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer" hx-get=@{TagNewR} hx-target="#tag-creator" hx-swap="innerHTML">
            + Ny kategori
        |]
    Just tid -> do
      existingTexts <- lookupPostParams "tags"
      let existingIds = mapMaybe (fmap toSqlKey . readMaybe . T.unpack) existingTexts :: [PostTagId]
          allSelectedIds = existingIds ++ [tid]
      renderFragment
        [hamlet|
          <button type="button" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer" hx-get=@{TagNewR} hx-target="#tag-creator" hx-swap="innerHTML">
            + Ny kategori
          <div hx-swap-oob="beforeend:#tag-list">
            <li id="tag-#{fromSqlKey tid}" class="flex items-center gap-3 text-sm">
              #{trimmed}
              <small>(0 innlegg)
              <button type="button" class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]"
                hx-get=@{TagByInt64EditR (fromSqlKey tid)}
                hx-target="#tag-#{fromSqlKey tid}"
                hx-swap="outerHTML">Rediger
              <button class="px-2 py-1 text-sm rounded cursor-pointer font-[inherit] bg-red-600 text-white"
                hx-post=@{TagByInt64DeleteR (fromSqlKey tid)}
                hx-target="#tag-#{fromSqlKey tid}"
                hx-swap="delete"
                hx-confirm="Sletta «#{trimmed}»? Dette fjernar taggen frå alle innlegg.">
                Slett
          <div hx-swap-oob="innerHTML:#selected-tags-holder">
            $forall sid <- allSelectedIds
              <input type="hidden" name="tags" value="#{fromSqlKey sid}">
        |]

getTagsInlineR :: Handler Html
getTagsInlineR = do
  _ <- requireLogin
  tags  <- runDB $ selectList [] [Asc PostTagTag]
  links <- runDB $ selectList [] []
  let countMap :: Map PostTagId Int
      countMap = foldr (\(Entity _ l) m -> Map.insertWith (+) (postTagLinkTagId l) 1 m) Map.empty links
  selectedTexts <- lookupGetParams "tags"
  let selectedIds = mapMaybe (fmap toSqlKey . readMaybe . T.unpack) selectedTexts :: [PostTagId]
  renderFragment
    [hamlet|
      <div id="selected-tags-holder">
        $forall sid <- selectedIds
          <input type="hidden" name="tags" value="#{fromSqlKey sid}">
      <ul id="tag-list" class="flex flex-col gap-2 list-none">
        $forall Entity tid tag <- tags
          <li id="tag-#{fromSqlKey tid}" class="flex items-center gap-3 text-sm">
            #{postTagTag tag}
            <small>(#{Map.findWithDefault 0 tid countMap} innlegg)
            <button type="button" class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]"
              hx-get=@{TagByInt64EditR (fromSqlKey tid)}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="outerHTML">Rediger
            <button class="px-2 py-1 text-sm rounded cursor-pointer font-[inherit] bg-red-600 text-white"
              hx-post=@{TagByInt64DeleteR (fromSqlKey tid)}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="delete"
              hx-confirm="Slette «#{postTagTag tag}»? Dette fjernar taggen frå alle innlegg.">
              Slett
      <div id="tag-creator" class="flex items-center gap-2 mt-2">
        <button type="button" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer" hx-get=@{TagNewR} hx-target="#tag-creator" hx-swap="innerHTML">
          + Ny kategori
      <button type="button" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer" hx-get=@{TagsSelectR} hx-target="#tags-area" hx-swap="innerHTML" hx-include="#selected-tags-holder input">
        Tilbake
    |]

getTagsSelectR :: Handler Html
getTagsSelectR = do
  _ <- requireLogin
  allTags <- runDB $ selectList [] [Asc PostTagTag]
  selectedTexts <- lookupGetParams "tags"
  let selectedIds = mapMaybe (fmap toSqlKey . readMaybe . T.unpack) selectedTexts :: [PostTagId]
  renderFragment
    [hamlet|
      <label class="block text-sm font-semibold mb-1.5">Kategori
      <div id="tags-select" class="flex flex-col gap-1">
        $forall Entity tid tag <- allTags
          <label class="flex items-center gap-2 text-sm cursor-pointer">
            <input type="checkbox" name="tags" value="#{fromSqlKey tid}" :elem tid selectedIds:checked>
            #{postTagTag tag}
      <button type="button" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer" hx-get=@{TagsInlineR} hx-target="#tags-area" hx-swap="innerHTML" hx-include="#tags-select input[type=checkbox]:checked">
        Rediger kategorier
    |]

getTagsR :: Handler Html
getTagsR = do
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
              hx-get=@{TagByInt64EditR (fromSqlKey tid)}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="outerHTML">Rediger
            <button class="px-2 py-1 text-sm rounded cursor-pointer font-[inherit] bg-red-600 text-white"
              hx-post=@{TagByInt64DeleteR (fromSqlKey tid)}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="delete"
              hx-confirm="Sletta «#{postTagTag tag}»? Dette fjernar taggen frå alle innlegg.">
              Slett
    |]

getTagByInt64EditR :: Int64 -> Handler Html
getTagByInt64EditR rawId = do
  let tid = toSqlKey rawId :: PostTagId
  _ <- requireLogin
  tag <- runDB (get tid) >>= maybe notFound pure
  renderFragment
    [hamlet|
      <li id="tag-#{rawId}" class="flex items-center gap-3 text-sm">
        <form class="flex items-center gap-2" hx-post=@{TagByInt64EditR rawId}
              hx-target="#tag-#{rawId}"
              hx-swap="outerHTML">
          <input type="text" name="tag" value="#{postTagTag tag}" required class="px-2 py-1 border rounded text-sm font-[inherit]">
          <button type="submit" class="px-2 py-1 text-sm rounded cursor-pointer font-[inherit] bg-green-600 text-white">Lagre
          <button type="button" class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit] text-red-600"
            hx-get=@{TagByInt64RowR rawId}
            hx-target="#tag-#{rawId}"
            hx-swap="outerHTML">Avbryt
    |]

postTagByInt64EditR :: Int64 -> Handler Html
postTagByInt64EditR rawId = do
  let tid = toSqlKey rawId :: PostTagId
  _ <- requireLogin
  newName <- runInputPost $ ireq textField "tag"
  let trimmed = T.strip newName
  mExisting <- runDB $ getBy (UniqueTag trimmed)
  case mExisting of
    Just (Entity existingId _) | existingId /= tid ->
      invalidArgs ["En kategori med dette navnet finnes allerede"]
    _ -> do
      runDB $ update tid [PostTagTag =. trimmed]
      n <- runDB $ length <$> selectList [PostTagLinkTagId ==. tid] []
      renderTagRow tid trimmed n

getTagByInt64RowR :: Int64 -> Handler Html
getTagByInt64RowR rawId = do
  let tid = toSqlKey rawId :: PostTagId
  _ <- requireLogin
  tag <- runDB (get tid) >>= maybe notFound pure
  n   <- runDB $ length <$> selectList [PostTagLinkTagId ==. tid] []
  renderTagRow tid (postTagTag tag) n

postTagByInt64DeleteR :: Int64 -> Handler Html
postTagByInt64DeleteR rawId = do
  let tid = toSqlKey rawId :: PostTagId
  _ <- requireLogin
  runDB $ do
    deleteWhere [PostTagLinkTagId ==. tid]
    delete tid
  sendResponseStatus ok200 ("" :: Text)

renderTagRow :: PostTagId -> Text -> Int -> Handler Html
renderTagRow tid name n =
  renderFragment
    [hamlet|
      <li id="tag-#{fromSqlKey tid}" class="flex items-center gap-3 text-sm">
        #{name}
        <small>(#{n} innlegg)
        <button type="button" class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]"
          hx-get=@{TagByInt64EditR (fromSqlKey tid)}
          hx-target="#tag-#{fromSqlKey tid}"
          hx-swap="outerHTML">Rediger
        <button class="px-2 py-1 text-sm rounded cursor-pointer font-[inherit] bg-red-600 text-white"
          hx-post=@{TagByInt64DeleteR (fromSqlKey tid)}
          hx-target="#tag-#{fromSqlKey tid}"
          hx-swap="delete"
          hx-confirm="Sletta «#{name}»? Dette fjernar taggen frå alle innlegg.">
          Slett
    |]
