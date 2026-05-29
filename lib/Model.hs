{-# LANGUAGE TypeOperators #-}

module Model where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Database.Persist.TH

data PostStatus = Wanted | Ordered | Bought
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

derivePersistField "PostStatus"

data DateStatus = Idea | Planned | Done
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

derivePersistField "DateStatus"

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
  email        Text
  name         Text
  passwordHash Text Maybe
  UniqueEmail  email
  deriving Show

Post
  createdAt    UTCTime
  status       PostStatus
  name         Text
  description  Text
  link         Text Maybe
  videoUrl     Text Maybe
  createdBy    UserId
  deletedAt    UTCTime Maybe
  deriving Show

DateIdea
  createdAt    UTCTime
  status       DateStatus
  title        Text
  description  Text
  location     Text Maybe
  createdBy    UserId
  deriving Show

PostImage
  postId       PostId
  filePath     Text
  sortOrder    Int
  deriving Show

PostTag
  tag          Text
  UniqueTag    tag

PostTagLink
  tagId        PostTagId
  postId       PostId

DateImage
  dateId       DateIdeaId
  filePath     Text
  sortOrder    Int
  deriving Show

DateTagLink
  tagId        PostTagId
  dateId       DateIdeaId

Itinerary
  dateId       DateIdeaId
  deriving Show

ItineraryItem
  itineraryId  ItineraryId
  name         Text
  description  Text Maybe
  location     Text Maybe
  start        UTCTime
  end          UTCTime
  deriving Show

|]
