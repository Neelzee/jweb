module Handler.Home where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Foundation
import Model
import Yesod

getHomeR :: Handler Html
getHomeR = do
  mStatusParam <- lookupGetParam "status"
  let mStatus = mStatusParam >>= parseStatus
  posts <- runDB $ case mStatus of
    Nothing -> selectList [PostDeletedAt ==. Nothing] [Desc PostCreatedAt]
    Just s -> selectList [PostStatus ==. s, PostDeletedAt ==. Nothing] [Desc PostCreatedAt]
  let pids = map entityKey posts
  images <- runDB $ selectList [PostImagePostId <-. pids] [Asc PostImageSortOrder]
  let imageMap = groupByPost images
  mUserId <- lookupSession "userId"
  let loggedIn = isJust mUserId
  defaultLayout $ do
    setTitle "Ønskeliste"
    [whamlet|
      <nav class="flex flex-wrap gap-1.5 mb-6 items-center">
        <a href=@{HomeR} class="px-3.5 py-1 rounded-full text-sm font-medium border transition-colors whitespace-nowrap no-underline">Alle
        <a href=@?{(HomeR, [("status", "wanted")])} class="px-3.5 py-1 rounded-full text-sm font-medium border transition-colors whitespace-nowrap no-underline">Ønsket
        <a href=@?{(HomeR, [("status", "ordered")])} class="px-3.5 py-1 rounded-full text-sm font-medium border transition-colors whitespace-nowrap no-underline">Bestilt
        <a href=@?{(HomeR, [("status", "bought")])} class="px-3.5 py-1 rounded-full text-sm font-medium border transition-colors whitespace-nowrap no-underline">Kjøpt
        $if loggedIn
          <a href=@{TrashR} class="filter-trash ml-auto px-3.5 py-1 rounded-full text-sm font-medium border transition-colors whitespace-nowrap no-underline">Papirkurv
      $if loggedIn
        <a href=@{PostNewR} class="inline-flex items-center px-4 py-2 rounded-lg text-sm font-semibold mb-6 shadow-sm transition no-underline">+ Nytt ønske
      <ul id="posts" class="list-none flex flex-col gap-3">
        $forall Entity pid post <- posts
          <li class="rounded-xl border p-5 shadow-sm transition">
            <div class="flex items-center gap-2 mb-2">
              <strong>#{postName post}
              <span class="status-#{statusVal (postStatus post)} text-xs font-bold uppercase tracking-widest px-2 py-0.5 rounded-full border">#{statusLabel (postStatus post)}
            $with imgs <- Map.findWithDefault [] pid imageMap
              $if not (null imgs)
                <div class="flex flex-wrap gap-2 mb-4">
                  $forall img <- imgs
                    <img src=@{UploadsR (postImageFilePath img)} alt="#{postName post}" class="w-28 h-28 object-cover rounded-lg border transition hover:scale-[1.04]">
            <p class="text-sm mb-2.5 leading-normal">#{postDescription post}
            $maybe link <- postLink post
              <a href="#{link}" target="_blank" rel="noopener" class="text-sm font-medium no-underline hover:underline underline-offset-2">Produkt link
            $maybe video <- postVideoUrl post
              <a href="#{video}" target="_blank" rel="noopener" class="text-sm font-medium no-underline hover:underline underline-offset-2">Video
            $if loggedIn
              <div class="flex items-center gap-3 mt-3.5 pt-3.5 border-t">
                <a href=@{PostEditR pid} class="text-sm font-medium no-underline transition-colors">Rediger
                <button class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer transition-colors"
                  hx-post=@{PostDeleteR pid}
                  hx-confirm="Slett ønsket?">Slett
    |]

groupByPost :: [Entity PostImage] -> Map PostId [PostImage]
groupByPost = foldr step Map.empty
  where
    step (Entity _ img) = Map.insertWith (++) (postImagePostId img) [img]

statusVal :: PostStatus -> Text
statusVal Wanted = "wanted"
statusVal Ordered = "ordered"
statusVal Bought = "bought"

parseStatus :: Text -> Maybe PostStatus
parseStatus "wanted" = Just Wanted
parseStatus "ordered" = Just Ordered
parseStatus "bought" = Just Bought
parseStatus _ = Nothing

statusLabel :: PostStatus -> Text
statusLabel Wanted = "Ønsket"
statusLabel Ordered = "Bestilt"
statusLabel Bought = "Kjøpt"
