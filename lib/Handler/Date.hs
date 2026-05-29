module Handler.Date where

import Control.Monad (forM_)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.Persist (insert, insert_)
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Foundation
import Model
import Network.HTTP.Types (ok200)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeExtension, (</>))
import Template.Date
import Yesod

getDateR :: Handler Html
getDateR = do
  _ <- requireLogin
  ideas <- runDB $ selectList [] [Desc DateIdeaCreatedAt]
  defaultLayout $ do
    setTitle "Dateliste"
    [whamlet|
      <div class="flex items-center justify-between mb-6">
        <h1 class="text-xl font-bold tracking-tight">Dater
        <a href=@{DateNewR} class="px-3 py-2 rounded-lg text-sm font-semibold no-underline border">Ny date
      $if null ideas
        <p class="text-sm text-gray-500">Ingen dateidear enno.
      $else
        <ul class="flex flex-col gap-4 list-none">
          $forall Entity did idea <- ideas
            <li class="border rounded-xl p-4">
              <div class="flex items-start justify-between gap-4">
                <div>
                  <a href=@{DateByInt64R (fromSqlKey did)} class="font-semibold no-underline">#{dateIdeaTitle idea}
                  $maybe loc <- dateIdeaLocation idea
                    <p class="text-sm text-gray-500 mt-0.5">#{loc}
                  <p class="text-xs text-gray-400 mt-1">#{statusLabel (dateIdeaStatus idea)}
                <a href=@{DateByInt64EditR (fromSqlKey did)} class="text-sm no-underline border rounded px-2 py-1">Rediger
    |]

getDateNewR :: Handler Html
getDateNewR = do
  _ <- requireLogin
  itinHtml <- renderFragment $ stagedItineraryFragment []
  defaultLayout $ do
    setTitle "Ny date"
    [whamlet|
      <h1 class="text-xl font-bold tracking-tight mb-6">Ny date
      <form class="flex flex-col gap-4 max-w-lg mx-auto" method="post" enctype="multipart/form-data">
        <label class="block text-sm font-semibold mb-1.5">Tittel
        <input type="text" name="title" required class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
        <label class="block text-sm font-semibold mb-1.5">Skildring
        <textarea name="description" class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition min-h-28 resize-y leading-normal">
        <label class="block text-sm font-semibold mb-1.5">Stad
        <input type="text" name="location" class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
        <label class="block text-sm font-semibold mb-1.5">Status
        <select name="status" class="w-full px-3.5 py-2.5 border rounded-lg text-base font-[inherit] transition">
          $forall (val, lbl) <- dateStatusOptions
            <option value="#{val}">#{lbl}
        <label class="block text-sm font-semibold mb-1.5">Bilete
        <input type="file" name="images" accept="image/*" multiple class="w-full px-3 py-2 text-sm font-[inherit] border rounded-lg cursor-pointer">
        <div>
          <p class="text-sm font-semibold mb-2">Program
          ^{itinHtml}
        <div class="flex items-center gap-4">
          <button type="submit" class="px-3 py-2 rounded-lg text-base font-semibold font-[inherit] border-0 cursor-pointer shadow-sm transition bg-green-600 text-white">Lagre
          <a href=@{DateR} class="text-sm font-medium no-underline text-red-600">Avbryt
    |]

postDateNewR :: Handler Html
postDateNewR = do
  uid <- requireLogin
  now <- liftIO getCurrentTime
  idea <- readDateForm now uid
  staged <- readStagedItems
  did <- runDB $ Database.Persist.insert idea
  handleDateImageUploads did
  case staged of
    [] -> pure ()
    items -> do
      iid <- runDB $ insert (Itinerary did)
      forM_ items $ \si ->
        case (parseDatetimeLocal (siStart si), parseDatetimeLocal (siEnd si)) of
          (Just start, Just end) ->
            runDB $ Database.Persist.insert_
              (ItineraryItem iid (siName si) (siDescription si) (siLocation si) start end)
          _ -> pure ()
  redirect DateR

postDateNewItineraryItemR :: Handler Html
postDateNewItineraryItemR = do
  _ <- requireLogin
  existing <- readStagedItems
  mName <- lookupPostParam "item-name"
  case mName of
    Nothing -> renderFragment $ stagedItineraryFragment existing
    Just rawName ->
      let name = T.strip rawName
       in if T.null name
            then renderFragment $ stagedItineraryFragment existing
            else do
              mDesc <- lookupPostParam "item-description"
              mLoc <- lookupPostParam "item-location"
              mStart <- lookupPostParam "item-start"
              mEnd <- lookupPostParam "item-end"
              case (mStart >>= parseDatetimeLocal, mEnd >>= parseDatetimeLocal) of
                (Just _, Just _) ->
                  let newItem =
                        StagedItem
                          name
                          (clean mDesc)
                          (clean mLoc)
                          (maybe "" id mStart)
                          (maybe "" id mEnd)
                   in renderFragment $ stagedItineraryFragment (existing ++ [newItem])
                _ -> renderFragment $ stagedItineraryFragment existing
  where
    clean Nothing = Nothing
    clean (Just t) = if T.null (T.strip t) then Nothing else Just t

postDateNewItineraryItemByIntR :: Int -> Handler Html
postDateNewItineraryItemByIntR idx = do
  _ <- requireLogin
  items <- readStagedItems
  let updated = [item | (i, item) <- zip [0 ..] items, i /= idx]
  renderFragment $ stagedItineraryFragment updated

getDateByInt64R :: Int64 -> Handler Html
getDateByInt64R rawId = do
  let did = toSqlKey rawId :: DateIdeaId
  _ <- requireLogin
  idea <- runDB (get did) >>= maybe notFound pure
  mItinerary <- runDB $ selectFirst [ItineraryDateId ==. did] []
  items <- case mItinerary of
    Nothing -> pure []
    Just (Entity iid _) -> runDB $ selectList [ItineraryItemItineraryId ==. iid] [Asc ItineraryItemStart]
  images <- runDB $ selectList [DateImageDateId ==. did] [Asc DateImageSortOrder]
  itinHtml <- renderFragment $ itineraryFragment rawId items
  defaultLayout $ do
    setTitle $ toHtml (dateIdeaTitle idea)
    [whamlet|
      <div class="flex items-start justify-between mb-6">
        <div>
          <h1 class="text-xl font-bold tracking-tight">#{dateIdeaTitle idea}
          <p class="text-sm text-gray-500 mt-1">#{statusLabel (dateIdeaStatus idea)}
          $maybe loc <- dateIdeaLocation idea
            <p class="text-sm text-gray-500">#{loc}
        <a href=@{DateByInt64EditR rawId} class="text-sm no-underline border rounded px-2 py-1">Rediger
      $if not (null images)
        <div class="flex gap-2 flex-wrap mb-6">
          $forall Entity _ img <- images
            <img src=@{UploadsByTextR (dateImageFilePath img)} class="h-32 w-auto rounded-lg object-cover">
      $if not (T.null (dateIdeaDescription idea))
        <p class="mb-6">#{dateIdeaDescription idea}
      ^{itinHtml}
    |]

getDateByInt64EditR :: Int64 -> Handler Html
getDateByInt64EditR rawId = do
  let did = toSqlKey rawId :: DateIdeaId
  _ <- requireLogin
  idea <- runDB (get did) >>= maybe notFound pure
  defaultLayout $ do
    setTitle "Rediger date"
    [whamlet|
      <h1 class="text-xl font-bold tracking-tight mb-6">Rediger date
      ^{dateForm (Just idea) (Just (toSqlKey rawId))}
    |]

postDateByInt64EditR :: Int64 -> Handler Html
postDateByInt64EditR rawId = do
  let did = toSqlKey rawId :: DateIdeaId
  uid <- requireLogin
  _ <- runDB (get did) >>= maybe notFound pure
  now <- liftIO getCurrentTime
  idea <- readDateForm now uid
  runDB $
    update
      did
      [ DateIdeaStatus =. dateIdeaStatus idea,
        DateIdeaTitle =. dateIdeaTitle idea,
        DateIdeaDescription =. dateIdeaDescription idea,
        DateIdeaLocation =. dateIdeaLocation idea
      ]
  handleDateImageUploads did
  redirect (DateByInt64R rawId)

postDateByInt64DeleteR :: Int64 -> Handler Html
postDateByInt64DeleteR rawId = do
  let did = toSqlKey rawId :: DateIdeaId
  _ <- requireLogin
  runDB $ do
    mItinerary <- selectFirst [ItineraryDateId ==. did] []
    forM_ mItinerary $ \(Entity iid _) -> do
      deleteWhere [ItineraryItemItineraryId ==. iid]
      delete iid
    deleteWhere [DateImageDateId ==. did]
    deleteWhere [DateTagLinkDateId ==. did]
    delete did
  addHeader "HX-Redirect" "/date"
  sendResponseStatus ok200 ("" :: Text)

postDateByInt64ItineraryItemR :: Int64 -> Handler Html
postDateByInt64ItineraryItemR rawId = do
  let did = toSqlKey rawId :: DateIdeaId
  _ <- requireLogin
  _ <- runDB (get did) >>= maybe notFound pure
  name <- runInputPost $ ireq textField "name"
  mDesc <- runInputPost $ iopt textField "description"
  mLoc <- runInputPost $ iopt textField "location"
  startText <- runInputPost $ ireq textField "start"
  endText <- runInputPost $ ireq textField "end"
  start <- maybe (invalidArgs ["Ugyldig starttid"]) pure (parseDatetimeLocal startText)
  end <- maybe (invalidArgs ["Ugyldig sluttid"]) pure (parseDatetimeLocal endText)
  let mDescNorm = mDesc >>= \t -> if T.null (T.strip t) then Nothing else Just t
      mLocNorm = mLoc >>= \t -> if T.null (T.strip t) then Nothing else Just t
  iid <- runDB $ do
    mItinerary <- selectFirst [ItineraryDateId ==. did] []
    case mItinerary of
      Just (Entity iid' _) -> pure iid'
      Nothing -> insert (Itinerary did)
  runDB $ insert_ (ItineraryItem iid name mDescNorm mLocNorm start end)
  items <- runDB $ selectList [ItineraryItemItineraryId ==. iid] [Asc ItineraryItemStart]
  renderFragment $ itineraryFragment rawId items

postDateByInt64ItineraryItemByInt64DeleteR :: Int64 -> Int64 -> Handler Html
postDateByInt64ItineraryItemByInt64DeleteR rawId rawItemId = do
  let did = toSqlKey rawId :: DateIdeaId
      itemId = toSqlKey rawItemId :: ItineraryItemId
  _ <- requireLogin
  runDB $ delete itemId
  mItinerary <- runDB $ selectFirst [ItineraryDateId ==. did] []
  items <- case mItinerary of
    Nothing -> pure []
    Just (Entity iid _) -> runDB $ selectList [ItineraryItemItineraryId ==. iid] [Asc ItineraryItemStart]
  renderFragment $ itineraryFragment rawId items

-- Helpers

readStagedItems :: Handler [StagedItem]
readStagedItems = go 0
  where
    go i = do
      let idx = T.pack (show (i :: Int))
      mName <- lookupPostParam ("item-name-" <> idx)
      case mName of
        Nothing -> pure []
        Just name -> do
          mDesc <- lookupPostParam ("item-description-" <> idx)
          mLoc <- lookupPostParam ("item-location-" <> idx)
          mStart <- lookupPostParam ("item-start-" <> idx)
          mEnd <- lookupPostParam ("item-end-" <> idx)
          rest <- go (i + 1)
          pure $
            StagedItem
              name
              (clean mDesc)
              (clean mLoc)
              (maybe "" id mStart)
              (maybe "" id mEnd)
              : rest
    clean Nothing = Nothing
    clean (Just t) = if T.null (T.strip t) then Nothing else Just t

parseDateStatusField :: Text -> DateStatus
parseDateStatusField "planned" = Planned
parseDateStatusField "done" = Done
parseDateStatusField _ = Idea

parseDatetimeLocal :: Text -> Maybe UTCTime
parseDatetimeLocal t =
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" (T.unpack t)

readDateForm :: UTCTime -> UserId -> Handler DateIdea
readDateForm now uid = do
  title <- runInputPost $ ireq textField "title"
  description <- maybe "" id <$> runInputPost (iopt textField "description")
  statusText <- runInputPost $ ireq textField "status"
  mLocRaw <- runInputPost $ iopt textField "location"
  let mLocation = mLocRaw >>= \t -> if T.null (T.strip t) then Nothing else Just t
  pure
    DateIdea
      { dateIdeaCreatedAt = now,
        dateIdeaStatus = parseDateStatusField statusText,
        dateIdeaTitle = title,
        dateIdeaDescription = description,
        dateIdeaLocation = mLocation,
        dateIdeaCreatedBy = uid
      }

handleDateImageUploads :: DateIdeaId -> Handler ()
handleDateImageUploads did = do
  files <- lookupFiles "images"
  uploadDir <- appUploadDir <$> getYesod
  liftIO $ createDirectoryIfMissing True uploadDir
  existing <- runDB $ selectList [DateImageDateId ==. did] [Asc DateImageSortOrder]
  let nextOrder = length existing
  mapM_ (saveDateImage did uploadDir nextOrder) (zip [0 ..] files)

saveDateImage :: DateIdeaId -> FilePath -> Int -> (Int, FileInfo) -> Handler ()
saveDateImage did dir baseOrder (i, fi) = do
  uuid <- liftIO nextRandom
  let ext = takeExtension (T.unpack (fileName fi))
      name = T.pack (toString uuid) <> T.pack ext
      dest = dir </> T.unpack name
  liftIO $ fileMove fi dest
  runDB $ Database.Persist.insert_ (DateImage did name (baseOrder + i))
