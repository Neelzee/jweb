module Handler.Post where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (unless, when)
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.Persist
  ( Entity (..)
  , SelectOpt (..)
  , get
  , insert
  , insert_
  , selectList
  , update
  , (=.)
  , (==.)
  )
import Foundation
import Model
import Network.HTTP.Types (ok200)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeExtension, (</>))
import Yesod

-- New post

getPostNewR :: Handler Html
getPostNewR = do
  _ <- requireLogin
  defaultLayout $ do
    setTitle "New Post"
    [whamlet|
      <h1>New post
      ^{postForm Nothing}
    |]

postPostNewR :: Handler Html
postPostNewR = do
  uid  <- requireLogin
  now  <- liftIO getCurrentTime
  post <- readPostForm
  pid  <- runDB $ insert (post now uid)
  handleImageUploads pid
  redirect HomeR

-- Edit post

getPostEditR :: PostId -> Handler Html
getPostEditR pid = do
  _ <- requireLogin
  post <- runDB (get pid) >>= maybe notFound pure
  defaultLayout $ do
    setTitle "Edit Post"
    [whamlet|
      <h1>Edit post
      ^{postForm (Just post)}
      <button
        hx-post=@{PostDeleteR pid}
        hx-confirm="Delete this post?">Slett ønske
    |]

postPostEditR :: PostId -> Handler Html
postPostEditR pid = do
  uid    <- requireLogin
  now    <- liftIO getCurrentTime
  mkPost <- readPostForm
  let p = mkPost now uid
  runDB $ update pid
    [ PostStatus      =. postStatus p
    , PostName        =. postName p
    , PostDescription =. postDescription p
    , PostLink        =. postLink p
    , PostVideoUrl    =. postVideoUrl p
    ]
  handleImageUploads pid
  redirect HomeR

-- Delete post

postPostDeleteR :: PostId -> Handler Html
postPostDeleteR pid = do
  _ <- requireLogin
  now <- liftIO getCurrentTime
  runDB $ update pid [PostDeletedAt =. Just now]
  addHeader "HX-Redirect" "/"
  sendResponseStatus ok200 ("" :: Text)

-- Serve uploads

getUploadsR :: Text -> Handler TypedContent
getUploadsR filename = do
  when (T.any (== '/') filename || T.isInfixOf ".." filename) notFound
  app  <- getYesod
  let path = appUploadDir app </> T.unpack filename
  exists <- liftIO $ doesFileExist path
  unless exists notFound
  sendFile (mimeFor path) path

-- Helpers

postForm :: Maybe Post -> Widget
postForm mPost = [whamlet|
  <form method="post" enctype="multipart/form-data">
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
|]
  where
    currentStatus = maybe "wanted" (statusVal . postStatus) mPost

statusOptions :: [(Text, Text)]
statusOptions =
  [ ("wanted",  "Wanted")
  , ("ordered", "Ordered")
  , ("bought",  "Bought")
  ]

statusVal :: PostStatus -> Text
statusVal Wanted  = "wanted"
statusVal Ordered = "ordered"
statusVal Bought  = "bought"

parseStatusField :: Text -> PostStatus
parseStatusField "ordered" = Ordered
parseStatusField "bought"  = Bought
parseStatusField _         = Wanted

readPostForm :: Handler (UTCTime -> UserId -> Post)
readPostForm = do
  name        <- runInputPost $ ireq textField "name"
  description <- runInputPost $ ireq textField "description"
  statusText  <- runInputPost $ ireq textField "status"
  mLink       <- runInputPost $ iopt urlField "link"
  mVideo      <- runInputPost $ iopt urlField "videoUrl"
  pure $ \now uid -> Post
    { postCreatedAt   = now
    , postStatus      = parseStatusField statusText
    , postName        = name
    , postDescription = description
    , postLink        = mLink
    , postVideoUrl    = mVideo
    , postCreatedBy   = uid
    , postDeletedAt   = Nothing
    }

handleImageUploads :: PostId -> Handler ()
handleImageUploads pid = do
  files     <- lookupFiles "images"
  uploadDir <- appUploadDir <$> getYesod
  liftIO $ createDirectoryIfMissing True uploadDir
  existing  <- runDB $ selectList [PostImagePostId ==. pid] [Asc PostImageSortOrder]
  let nextOrder = length existing
  mapM_ (saveImage pid uploadDir nextOrder) (zip [0..] files)

saveImage :: PostId -> FilePath -> Int -> (Int, FileInfo) -> Handler ()
saveImage pid dir baseOrder (i, fi) = do
  uuid <- liftIO nextRandom
  let ext  = takeExtension (T.unpack (fileName fi))
      name = T.pack (toString uuid) <> T.pack ext
      dest = dir </> T.unpack name
  liftIO $ fileMove fi dest
  runDB $ insert_ (PostImage pid name (baseOrder + i))

mimeFor :: FilePath -> ContentType
mimeFor path = case takeExtension path of
  ".jpg"  -> "image/jpeg"
  ".jpeg" -> "image/jpeg"
  ".png"  -> "image/png"
  ".gif"  -> "image/gif"
  ".webp" -> "image/webp"
  _       -> "application/octet-stream"
