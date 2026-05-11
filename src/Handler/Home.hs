module Handler.Home where

import Data.Maybe (isJust)
import Data.Text (Text)
import Database.Persist (Entity (..), SelectOpt (..), selectList, (==.))
import Foundation
import Model
import Yesod

getHomeR :: Handler Html
getHomeR = do
  mStatusParam <- lookupGetParam "status"
  let mStatus = mStatusParam >>= parseStatus
  posts <- runDB $ case mStatus of
    Nothing -> selectList [] [Desc PostCreatedAt]
    Just s  -> selectList [PostStatus ==. s] [Desc PostCreatedAt]
  mUserId <- lookupSession "userId"
  let loggedIn = isJust mUserId
  defaultLayout $ do
    setTitle "Kitchen Wishlist"
    [whamlet|
      <nav .filters>
        <a href=@{HomeR}>All
        <a href=@?{(HomeR, [("status", "wanted")])}>Wanted
        <a href=@?{(HomeR, [("status", "ordered")])}>Ordered
        <a href=@?{(HomeR, [("status", "bought")])}>Bought
      $if loggedIn
        <a href=@{PostNewR} .btn-new>+ New post
      <ul #posts>
        $forall Entity pid post <- posts
          <li>
            <div .post-header>
              <strong>#{postName post}
              <span .status>#{statusLabel (postStatus post)}
            <p .description>#{postDescription post}
            $maybe link <- postLink post
              <a href="#{link}" target="_blank" rel="noopener">Product link
            $maybe video <- postVideoUrl post
              <a href="#{video}" target="_blank" rel="noopener">Video
            $if loggedIn
              <div .post-actions>
                <a href=@{PostEditR pid}>Edit
                <button
                  hx-post=@{PostDeleteR pid}
                  hx-confirm="Delete this post?">Delete
    |]

parseStatus :: Text -> Maybe PostStatus
parseStatus "wanted"  = Just Wanted
parseStatus "ordered" = Just Ordered
parseStatus "bought"  = Just Bought
parseStatus _         = Nothing

statusLabel :: PostStatus -> Text
statusLabel Wanted  = "Wanted"
statusLabel Ordered = "Ordered"
statusLabel Bought  = "Bought"
