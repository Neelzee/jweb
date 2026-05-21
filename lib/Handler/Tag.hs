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

getTagNewR :: Handler Html
getTagNewR = do
  _ <- requireLogin
  renderFragment
    [hamlet|
      <form class="flex items-center gap-2" hx-post=@{TagCreateR} hx-target="#tag-creator" hx-swap="innerHTML">
        <input type="text" name="tag" required placeholder="Ny kategori" class="px-2 py-1 border rounded text-sm font-[inherit]">
        <button type="submit" class="px-2 py-1 text-sm rounded cursor-pointer font-[inherit] bg-green-600 text-white">Opprett
    |]

postTagCreateR :: Handler Html
postTagCreateR = do
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
    Just tid ->
      renderFragment
        [hamlet|
          <button type="button" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer" hx-get=@{TagNewR} hx-target="#tag-creator" hx-swap="innerHTML">
            + Ny kategori
          <div hx-swap-oob="beforeend:#tag-list">
            <li id="tag-#{fromSqlKey tid}" class="flex items-center gap-3 text-sm">
              #{trimmed}
              <small>(0 innlegg)
              <button type="button" class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]"
                hx-get=@{TagEditR tid}
                hx-target="#tag-#{fromSqlKey tid}"
                hx-swap="outerHTML">Rediger
              <button class="px-2 py-1 text-sm rounded cursor-pointer font-[inherit] bg-red-600 text-white"
                hx-post=@{TagDeleteR tid}
                hx-target="#tag-#{fromSqlKey tid}"
                hx-swap="delete"
                hx-confirm="Sletta «#{trimmed}»? Dette fjernar taggen frå alle innlegg.">
                Slett
        |]

getTagInlineR :: Handler Html
getTagInlineR = do
  _ <- requireLogin
  tags  <- runDB $ selectList [] [Asc PostTagTag]
  links <- runDB $ selectList [] []
  let countMap :: Map PostTagId Int
      countMap = foldr (\(Entity _ l) m -> Map.insertWith (+) (postTagLinkTagId l) 1 m) Map.empty links
  renderFragment
    [hamlet|
      <ul id="tag-list" class="flex flex-col gap-2 list-none">
        $forall Entity tid tag <- tags
          <li id="tag-#{fromSqlKey tid}" class="flex items-center gap-3 text-sm">
            #{postTagTag tag}
            <small>(#{Map.findWithDefault 0 tid countMap} innlegg)
            <button type="button" class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit]"
              hx-get=@{TagEditR tid}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="outerHTML">Rediger
            <button class="px-2 py-1 text-sm rounded cursor-pointer font-[inherit] bg-red-600 text-white"
              hx-post=@{TagDeleteR tid}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="delete"
              hx-confirm="Slette «#{postTagTag tag}»? Dette fjernar taggen frå alle innlegg.">
              Slett
      <div id="tag-creator" class="flex items-center gap-2 mt-2">
        <button type="button" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer" hx-get=@{TagNewR} hx-target="#tag-creator" hx-swap="innerHTML">
          + Ny kategori
      <button type="button" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer" hx-get=@{TagSelectR} hx-target="#tags-area" hx-swap="innerHTML">
        Tilbake
    |]

getTagSelectR :: Handler Html
getTagSelectR = do
  _ <- requireLogin
  allTags <- runDB $ selectList [] [Asc PostTagTag]
  renderFragment
    [hamlet|
      <label class="block text-sm font-semibold mb-1.5">Kategori
      <div id="tags-select" class="flex flex-col gap-1">
        $forall Entity tid tag <- allTags
          <label class="flex items-center gap-2 text-sm cursor-pointer">
            <input type="checkbox" name="tags" value="#{fromSqlKey tid}">
            #{postTagTag tag}
      <button type="button" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer" hx-get=@{TagInlineR} hx-target="#tags-area" hx-swap="innerHTML">
        Rediger kategorier
    |]

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
            <button class="px-2 py-1 text-sm rounded cursor-pointer font-[inherit] bg-red-600 text-white"
              hx-post=@{TagDeleteR tid}
              hx-target="#tag-#{fromSqlKey tid}"
              hx-swap="delete"
              hx-confirm="Sletta «#{postTagTag tag}»? Dette fjernar taggen frå alle innlegg.">
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
          <button type="submit" class="px-2 py-1 text-sm rounded cursor-pointer font-[inherit] bg-green-600 text-white">Lagre
          <button type="button" class="px-2 py-1 text-sm rounded border cursor-pointer font-[inherit] text-red-600"
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
        <button class="px-2 py-1 text-sm rounded cursor-pointer font-[inherit] bg-red-600 text-white"
          hx-post=@{TagDeleteR tid}
          hx-target="#tag-#{fromSqlKey tid}"
          hx-swap="delete"
          hx-confirm="Sletta «#{name}»? Dette fjernar taggen frå alle innlegg.">
          Slett
    |]
