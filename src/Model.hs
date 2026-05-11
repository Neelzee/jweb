{-# LANGUAGE TypeOperators #-}

module Model where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Database.Persist.TH

data PostStatus = Wanted | Ordered | Bought
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

derivePersistField "PostStatus"

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
  createdAt   UTCTime
  status      PostStatus
  name        Text
  description Text
  link        Text Maybe
  videoUrl    Text Maybe
  createdBy   UserId
  deletedAt   UTCTime Maybe
  deriving Show

PostImage
  postId    PostId
  filePath  Text
  sortOrder Int
  deriving Show
|]
