module Handler.Home where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Foundation
import Model
import Text.Read (readMaybe)
import Yesod

getJwebHomeR :: Handler Html
getJwebHomeR = do
  mStatusParam <- lookupGetParam "status"
  mTagParam    <- lookupGetParam "tag"
  let mStatus = mStatusParam >>= parseStatus
      mTagId  = mTagParam >>= \t -> toSqlKey <$> (readMaybe (T.unpack t) :: Maybe Int64)

  posts <- runDB $ case mTagId of
    Nothing  -> case mStatus of
      Nothing -> selectList [PostDeletedAt ==. Nothing] [Desc PostCreatedAt]
      Just s  -> selectList [PostStatus ==. s, PostDeletedAt ==. Nothing] [Desc PostCreatedAt]
    Just tid -> do
      links <- selectList [PostTagLinkTagId ==. tid] []
      let linkedPids = map (postTagLinkPostId . entityVal) links
      case mStatus of
        Nothing -> selectList [PostId <-. linkedPids, PostDeletedAt ==. Nothing] [Desc PostCreatedAt]
        Just s  -> selectList [PostStatus ==. s, PostId <-. linkedPids, PostDeletedAt ==. Nothing] [Desc PostCreatedAt]

  let pids = map entityKey posts

  images     <- runDB $ selectList [PostImagePostId <-. pids] [Asc PostImageSortOrder]
  allTags    <- runDB $ selectList [] [Asc PostTagTag]
  tagLinks   <- runDB $ selectList [PostTagLinkPostId <-. pids] []
  let tagLinkVals = map entityVal tagLinks
  tagEnts    <- runDB $ selectList [PostTagId <-. map postTagLinkTagId tagLinkVals] []

  let imageMap      = groupByPost images
      tagMap        = buildTagMap tagLinkVals tagEnts
      tagParam      = maybe [] (\t -> [("tag", t)]) mTagParam
      wantedParams  = ("status", "wanted")  : tagParam
      orderedParams = ("status", "ordered") : tagParam
      boughtParams  = ("status", "bought")  : tagParam

  mUserId <- lookupSession "userId"
  let loggedIn = isJust mUserId

  defaultLayout $ do
    setTitle "Ønskeliste"
    [whamlet|
      <div class="flex flex-col sm:flex-row gap-6 sm:gap-8">
        <aside class="w-44 shrink-0">
          <p class="text-xs font-semibold uppercase tracking-widest mb-3">Kategori
          <form method="get" action=@{JwebHomeR}>
            $maybe status <- mStatusParam
              <input type="hidden" name="status" value="#{status}">
            <select class="w-full border rounded-lg px-3 py-2 text-sm font-[inherit] cursor-pointer"
                    name="tag"
                    onchange="this.form.submit()">
              <option value="" :mTagId == Nothing:selected>Alle
              $forall Entity tid tag <- allTags
                <option value="#{fromSqlKey tid}" :mTagId == Just tid:selected>#{postTagTag tag}
        <div class="flex-1 min-w-0">
          <nav class="flex flex-wrap gap-1.5 mb-6 items-center">
            <a href=@?{(JwebHomeR, tagParam)} class="px-3.5 py-1 rounded-full text-sm font-medium border transition-colors whitespace-nowrap no-underline">Alle
            <a href=@?{(JwebHomeR, wantedParams)} class="px-3.5 py-1 rounded-full text-sm font-medium border transition-colors whitespace-nowrap no-underline">Ønsket
            <a href=@?{(JwebHomeR, orderedParams)} class="px-3.5 py-1 rounded-full text-sm font-medium border transition-colors whitespace-nowrap no-underline">Bestilt
            <a href=@?{(JwebHomeR, boughtParams)} class="px-3.5 py-1 rounded-full text-sm font-medium border transition-colors whitespace-nowrap no-underline">Kjøpt
            $if loggedIn
              <a href=@{TrashR} class="filter-trash hidden sm:inline-flex ml-auto px-3.5 py-1 rounded-full text-sm font-medium border transition-colors whitespace-nowrap no-underline">Papirkurv
          $if loggedIn
            <a href=@{PostNewR} class="hidden sm:inline-flex items-center px-4 py-2 rounded-lg text-sm font-semibold mb-6 shadow-sm transition no-underline">+ Nytt ønske
            <a href=@{PostNewR} class="sm:hidden fixed bottom-6 right-6 z-50 w-14 h-14 rounded-full flex items-center justify-center text-2xl font-bold no-underline shadow-lg bg-green-600 text-white">+
          <ul id="posts" class="list-none flex flex-col gap-3">
            $forall Entity pid post <- posts
              <li class="max-w-[52rem] rounded-xl border p-5 shadow-sm transition">
                <div class="flex items-center gap-2 mb-2">
                  <strong>#{postName post}
                  <span class="status-#{statusVal (postStatus post)} text-xs font-bold uppercase tracking-widest px-2 py-0.5 rounded-full border">#{statusLabel (postStatus post)}
                  $with postTags <- Map.findWithDefault [] pid tagMap
                    $if not (null postTags)
                      <div class="ml-auto flex flex-wrap gap-1">
                        $forall tagName <- postTags
                          <span class="text-xs px-2 py-0.5 rounded-full border">#{tagName}
                $with imgs <- Map.findWithDefault [] pid imageMap
                  $if not (null imgs)
                    <div class="flex flex-wrap gap-2 mb-4">
                      $forall img <- imgs
                        <img src=@{UploadsByTextR (postImageFilePath img)} alt="#{postName post}" class="w-28 h-28 object-cover rounded-lg border transition hover:scale-[1.04]">
                <p class="text-sm mb-2.5 leading-normal">#{postDescription post}
                $maybe link <- postLink post
                  <a href="#{link}" target="_blank" rel="noopener" class="text-sm font-medium no-underline hover:underline underline-offset-2">Produkt link
                $maybe video <- postVideoUrl post
                  <a href="#{video}" target="_blank" rel="noopener" class="text-sm font-medium no-underline hover:underline underline-offset-2">Video
                $if loggedIn
                  <div class="flex items-center gap-3 mt-3.5 pt-3.5 border-t">
                    <a href=@{PostByInt64EditR (fromSqlKey pid)} class="text-sm font-medium no-underline transition-colors">Rediger
                    <button class="text-sm font-medium font-[inherit] px-2 py-0.5 rounded border-0 cursor-pointer transition-colors bg-red-600 text-white"
                      hx-post=@{PostByInt64DeleteR (fromSqlKey pid)}
                      hx-confirm="Slett ønsket?">Slett
    |]

groupByPost :: [Entity PostImage] -> Map PostId [PostImage]
groupByPost = foldr step Map.empty
  where
    step (Entity _ img) = Map.insertWith (++) (postImagePostId img) [img]

buildTagMap :: [PostTagLink] -> [Entity PostTag] -> Map PostId [Text]
buildTagMap links tagEnts = foldr step Map.empty links
  where
    tagById = Map.fromList [(entityKey t, postTagTag (entityVal t)) | t <- tagEnts]
    step link m = case Map.lookup (postTagLinkTagId link) tagById of
      Nothing   -> m
      Just name -> Map.insertWith (++) (postTagLinkPostId link) [name] m

statusVal :: PostStatus -> Text
statusVal Wanted  = "wanted"
statusVal Ordered = "ordered"
statusVal Bought  = "bought"

parseStatus :: Text -> Maybe PostStatus
parseStatus "wanted"  = Just Wanted
parseStatus "ordered" = Just Ordered
parseStatus "bought"  = Just Bought
parseStatus _         = Nothing

statusLabel :: PostStatus -> Text
statusLabel Wanted  = "Ønsket"
statusLabel Ordered = "Bestilt"
statusLabel Bought  = "Kjøpt"
