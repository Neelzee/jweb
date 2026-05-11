module Handler.Home where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Database.Persist (Entity (..), SelectOpt (..), selectList, (==.), (<-.))
import Foundation
import Model
import Yesod

getHomeR :: Handler Html
getHomeR = do
  mStatusParam <- lookupGetParam "status"
  let mStatus = mStatusParam >>= parseStatus
  posts <- runDB $ case mStatus of
    Nothing -> selectList [PostDeletedAt ==. Nothing] [Desc PostCreatedAt]
    Just s  -> selectList [PostStatus ==. s, PostDeletedAt ==. Nothing] [Desc PostCreatedAt]
  let pids = map entityKey posts
  images <- runDB $ selectList [PostImagePostId <-. pids] [Asc PostImageSortOrder]
  let imageMap = groupByPost images
  mUserId <- lookupSession "userId"
  let loggedIn = isJust mUserId
  defaultLayout $ do
    setTitle "Ønskeliste"
    [whamlet|
      <nav .filters>
        <a href=@{HomeR}>All
        <a href=@?{(HomeR, [("status", "wanted")])}>Ønsket
        <a href=@?{(HomeR, [("status", "ordered")])}>Bestilt
        <a href=@?{(HomeR, [("status", "bought")])}>Kjøpt
        $if loggedIn
          <a href=@{TrashR} .filter-trash>Papirkurv
      $if loggedIn
        <a href=@{PostNewR} .btn-new>+ Nytt ønske
      <ul #posts>
        $forall Entity pid post <- posts
          <li>
            <div .post-header>
              <strong>#{postName post}
              <span .status class="status-#{statusVal (postStatus post)}">#{statusLabel (postStatus post)}
            $with imgs <- Map.findWithDefault [] pid imageMap
              $if not (null imgs)
                <div .post-images>
                  $forall img <- imgs
                    <img src=@{UploadsR (postImageFilePath img)} alt="#{postName post}">
            <p .description>#{postDescription post}
            $maybe link <- postLink post
              <a href="#{link}" target="_blank" rel="noopener">Produkt link
            $maybe video <- postVideoUrl post
              <a href="#{video}" target="_blank" rel="noopener">Video
            $if loggedIn
              <div .post-actions>
                <a href=@{PostEditR pid}>Rediger
                <button
                  hx-post=@{PostDeleteR pid}
                  hx-confirm="Delete this post?">Slett
    |]

groupByPost :: [Entity PostImage] -> Map PostId [PostImage]
groupByPost = foldr step Map.empty
  where
    step (Entity _ img) = Map.insertWith (++) (postImagePostId img) [img]

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
