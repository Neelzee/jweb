module Handler.Post where

import Control.Monad (unless, when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import Database.Persist
  ( SelectOpt (..)
  , get
  , insert
  , insert_
  , selectList
  , update
  , (=.)
  , (==.)
  )
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Foundation
import Model
import Network.HTTP.Types (ok200)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeExtension, (</>))
import Yesod

getPostNewR :: Handler Html
getPostNewR = do
  _ <- requireLogin
  allTags <- runDB $ selectList [] []
  defaultLayout $ do
    setTitle "Nytt ønske"
    [whamlet|
      <h1>New post
      ^{postForm Nothing allTags []}
    |]

postPostNewR :: Handler Html
postPostNewR = do
  uid <- requireLogin
  now <- liftIO getCurrentTime
  (post, tagIds) <- readPostForm
  pid <- runDB $ Database.Persist.insert (post now uid)
  runDB $ mapM_ (\tid -> insert_ (PostTagLink tid pid)) tagIds
  handleImageUploads pid
  redirect HomeR

getPostEditR :: PostId -> Handler Html
getPostEditR pid = do
  _ <- requireLogin
  post <- runDB (Database.Persist.get pid) >>= maybe notFound pure
  allTags <- runDB $ selectList [] []
  links <- runDB $ selectList [PostTagLinkPostId ==. pid] []
  let selectedIds = map (postTagLinkTagId . entityVal) links
  defaultLayout $ do
    setTitle "Rediger ønske"
    [whamlet|
      <h1>Edit post
      ^{postForm (Just post) allTags selectedIds}
      <button
        hx-post=@{PostDeleteR pid}
        hx-confirm="Delete this post?">Slett ønske
    |]

postPostEditR :: PostId -> Handler Html
postPostEditR pid = do
  uid <- requireLogin
  now <- liftIO getCurrentTime
  (mkPost, tagIds) <- readPostForm
  runDB $ deleteWhere [PostTagLinkPostId ==. pid]
  runDB $ mapM_ (\tid -> insert_ (PostTagLink tid pid)) tagIds
  let p = mkPost now uid
  runDB $
    Database.Persist.update
      pid
      [ PostStatus Database.Persist.=. postStatus p,
        PostName Database.Persist.=. postName p,
        PostDescription Database.Persist.=. postDescription p,
        PostLink Database.Persist.=. postLink p,
        PostVideoUrl Database.Persist.=. postVideoUrl p
      ]
  handleImageUploads pid
  redirect HomeR

postPostDeleteR :: PostId -> Handler Html
postPostDeleteR pid = do
  _ <- requireLogin
  now <- liftIO getCurrentTime
  runDB $ Database.Persist.update pid [PostDeletedAt Database.Persist.=. Just now]
  addHeader "HX-Redirect" "/"
  sendResponseStatus ok200 ("" :: Text)

getUploadsR :: Text -> Handler TypedContent
getUploadsR filename = do
  when (T.any (== '/') filename || T.isInfixOf ".." filename) notFound
  app <- getYesod
  let path = appUploadDir app </> T.unpack filename
  exists' <- liftIO $ doesFileExist path
  unless exists' notFound
  sendFile (mimeFor path) path

-- Helpers

postForm :: Maybe Post -> [Entity PostTag] -> [PostTagId] -> Widget
postForm mPost allTags selectedIds =
  [whamlet|
  <form method="post" enctype="multipart/form-data">
    <label>Kategori
    <select id="tags-select" name="tags" multiple>
      $forall (tagKey, tagName, isSelected) <- tagData
        <option value="#{tagKey}" id="tag-option-#{tagKey}" class="tag-option" :isSelected:selected>#{tagName}
    <div id="tag-creator">
      <button type="button" hx-get=@{TagNewR} hx-target="#tag-creator" hx-swap="innerHTML">
        + Ny kategori
    <a href=@{TagListR}>Rediger kategorier
    <label>Namn
    <input type="text" name="name" value="#{maybe "" postName mPost}" required>
    <label>Beskrivelse
    <textarea name="description">#{maybe "" postDescription mPost}
    <label>Status
    <select name="status">
      $forall (val, lbl) <- statusOptions
        <option value="#{val}" :currentStatus == val:selected>#{lbl}
    <label>Produkt link
    <input type="url" name="link" value="#{maybe "" (maybe "" id . postLink) mPost}">
    <label>Video URL
    <input type="url" name="videoUrl" value="#{maybe "" (maybe "" id . postVideoUrl) mPost}">
    <label>Bilder
    <input type="file" name="images" accept="image/*" multiple>
    <button type="submit">Lagre
    <a href="/">Avbryt
|]
  where
    currentStatus = maybe "wanted" (statusVal . postStatus) mPost
    tagData :: [(Int64, Text, Bool)]
    tagData = map (\(Entity tid ptag) -> (fromSqlKey tid, postTagTag ptag, tid `elem` selectedIds)) allTags

statusOptions :: [(Text, Text)]
statusOptions =
  [ ("wanted", "Ønsket"),
    ("ordered", "Besilt"),
    ("bought", "Kjøpt")
  ]

statusVal :: PostStatus -> Text
statusVal Wanted = "wanted"
statusVal Ordered = "ordered"
statusVal Bought = "bought"

parseStatusField :: Text -> PostStatus
parseStatusField "ordered" = Ordered
parseStatusField "bought" = Bought
parseStatusField _ = Wanted

readPostForm :: Handler (UTCTime -> UserId -> Post, [PostTagId])
readPostForm = do
  name <- runInputPost $ ireq textField "name"
  description <- maybe "" id <$> runInputPost (iopt textField "description")
  statusText <- runInputPost $ ireq textField "status"
  mLink <- runInputPost $ iopt urlField "link"
  mVideo <- runInputPost $ iopt urlField "videoUrl"
  tagTexts <- lookupPostParams "tags"
  let tagIds = mapMaybe (fmap toSqlKey . readMaybe . T.unpack) tagTexts
  pure
    ( \now uid ->
        Post
          { postCreatedAt = now,
            postStatus = parseStatusField statusText,
            postName = name,
            postDescription = description,
            postLink = mLink,
            postVideoUrl = mVideo,
            postCreatedBy = uid,
            postDeletedAt = Nothing
          }
    , tagIds
    )

handleImageUploads :: PostId -> Handler ()
handleImageUploads pid = do
  files <- lookupFiles "images"
  uploadDir <- appUploadDir <$> getYesod
  liftIO $ createDirectoryIfMissing True uploadDir
  existing <-
    runDB $
      Database.Persist.selectList
        [PostImagePostId Database.Persist.==. pid]
        [Database.Persist.Asc PostImageSortOrder]
  let nextOrder = length existing
  mapM_ (saveImage pid uploadDir nextOrder) (zip [0 ..] files)

saveImage :: PostId -> FilePath -> Int -> (Int, FileInfo) -> Handler ()
saveImage pid dir baseOrder (i, fi) = do
  uuid <- liftIO nextRandom
  let ext = takeExtension (T.unpack (fileName fi))
      name = T.pack (toString uuid) <> T.pack ext
      dest = dir </> T.unpack name
  liftIO $ fileMove fi dest
  runDB $ Database.Persist.insert_ (PostImage pid name (baseOrder + i))

mimeFor :: FilePath -> ContentType
mimeFor path = case takeExtension path of
  ".jpg"  -> "image/jpeg"
  ".jpeg" -> "image/jpeg"
  ".png"  -> "image/png"
  ".gif"  -> "image/gif"
  ".webp" -> "image/webp"
  _       -> "application/octet-stream"
