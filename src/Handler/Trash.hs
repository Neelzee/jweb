module Handler.Trash where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Foundation
import Database.Persist (Entity (..), SelectOpt (..), selectList, update, (!=.), (==.), (<-.))
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
      <h1>Papirkurv
      $if null posts
        <p .muted>Ingen slettede ønsker.
      $else
        <ul #posts>
          $forall Entity pid post <- posts
            <li>
              <div .post-header>
                <strong>#{postName post}
                <span .status .status-deleted>Slettet
              $with imgs <- Map.findWithDefault [] pid imageMap
                $if not (null imgs)
                  <div .post-images>
                    $forall img <- imgs
                      <img src=@{UploadsR (postImageFilePath img)} alt="#{postName post}">
              <p .description>#{postDescription post}
              <div .post-actions>
                <form method="post" action=@{PostRestoreR pid}>
                  $maybe token <- mToken
                    <input type="hidden" name="_token" value="#{token}">
                  <button type="submit">Gjenopprett
    |]

postPostRestoreR :: PostId -> Handler ()
postPostRestoreR pid = do
  _ <- requireLogin
  runDB $ update pid [PostDeletedAt =. Nothing]
  redirect TrashR

groupByPost :: [Entity PostImage] -> Map PostId [PostImage]
groupByPost = foldr step Map.empty
  where
    step (Entity _ img) = Map.insertWith (++) (postImagePostId img) [img]
