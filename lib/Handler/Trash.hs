module Handler.Trash where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Foundation
import Model
import Yesod

getTrashR :: Handler Html
getTrashR = do
  _ <- requireLogin
  req <- getRequest
  let mToken = reqToken req
  posts <- runDB $ selectList [PostDeletedAt !=. Nothing] [Desc PostDeletedAt]
  let pids = map entityKey posts
  images <- runDB $ selectList [PostImagePostId <-. pids] [Asc PostImageSortOrder]
  let imageMap = groupByPost images
  defaultLayout $ do
    setTitle "Papirkurv"
    [whamlet|
      <h1 class="text-xl font-bold tracking-tight mb-6">Papirkurv
      $if null posts
        <p class="muted text-sm">Ingen slettede ønsker.
      $else
        <ul id="posts" class="list-none flex flex-col gap-3">
          $forall Entity pid post <- posts
            <li class="rounded-xl border p-5 shadow-sm transition">
              <div class="flex items-center gap-2 mb-2">
                <strong>#{postName post}
                <span class="status-deleted text-xs font-bold uppercase tracking-widest px-2 py-0.5 rounded-full border">Slettet
              $with imgs <- Map.findWithDefault [] pid imageMap
                $if not (null imgs)
                  <div class="flex flex-wrap gap-2 mb-4">
                    $forall img <- imgs
                      <img src=@{UploadsByTextR (postImageFilePath img)} alt="#{postName post}" class="w-28 h-28 object-cover rounded-lg border transition hover:scale-[1.04]">
              <p class="text-sm mb-2.5 leading-normal">#{postDescription post}
              <div class="flex items-center gap-3 mt-3.5 pt-3.5 border-t">
                <form class="contents" method="post" action=@{PostByInt64RestoreR (fromSqlKey pid)}>
                  $maybe token <- mToken
                    <input type="hidden" name="_token" value="#{token}">
                  <button type="submit" class="text-sm font-medium font-[inherit] bg-transparent border-0 p-0 cursor-pointer transition-colors">Gjenopprett
    |]

postPostByInt64RestoreR :: Int64 -> Handler ()
postPostByInt64RestoreR rawId = do
  let pid = toSqlKey rawId :: PostId
  _ <- requireLogin
  runDB $ update pid [PostDeletedAt =. Nothing]
  redirect TrashR

groupByPost :: [Entity PostImage] -> Map PostId [PostImage]
groupByPost = foldr step Map.empty
  where
    step (Entity _ img) = Map.insertWith (++) (postImagePostId img) [img]
