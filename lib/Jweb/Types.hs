{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Jweb.Types (
  AppStatus (..),
  Post (..),
  PostStatus (..),
  Tag (..),
  User (..),
  ) where

import ClassyPrelude.Yesod
import Data.Foldable (foldl)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)


-- | 
data AppStatus = AppStatus
  { appStatusVersion :: Text -- ^ Semantic version (e.g. \"1.2.3\")
  , appStatusStartedAt :: UTCTime -- ^ 
  , appStatusUptimeSeconds :: Int64 -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON AppStatus where
  parseJSON = genericParseJSON optionsAppStatus
instance ToJSON AppStatus where
  toJSON = genericToJSON optionsAppStatus

optionsAppStatus :: Options
optionsAppStatus =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ List.lookup s table
    }
  where
    table =
      [ ("appStatusVersion", "version")
      , ("appStatusStartedAt", "startedAt")
      , ("appStatusUptimeSeconds", "uptimeSeconds")
      ]


-- | 
data Post = Post
  { postId :: Int64 -- ^ 
  , postName :: Text -- ^ 
  , postDescription :: Text -- ^ 
  , postStatus :: PostStatus -- ^ 
  , postTags :: [Text] -- ^ 
  , postImageUrls :: [Text] -- ^ 
  , postLink :: Maybe Text -- ^ 
  , postVideoUrl :: Maybe Text -- ^ 
  , postCreatedAt :: UTCTime -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Post where
  parseJSON = genericParseJSON optionsPost
instance ToJSON Post where
  toJSON = genericToJSON optionsPost

optionsPost :: Options
optionsPost =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ List.lookup s table
    }
  where
    table =
      [ ("postId", "id")
      , ("postName", "name")
      , ("postDescription", "description")
      , ("postStatus", "status")
      , ("postTags", "tags")
      , ("postImageUrls", "imageUrls")
      , ("postLink", "link")
      , ("postVideoUrl", "videoUrl")
      , ("postCreatedAt", "createdAt")
      ]


-- | 
data PostStatus = PostStatus
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON PostStatus where
  parseJSON = genericParseJSON optionsPostStatus
instance ToJSON PostStatus where
  toJSON = genericToJSON optionsPostStatus

optionsPostStatus :: Options
optionsPostStatus =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ List.lookup s table
    }
  where
    table =
      [ 
      ]


-- | 
data Tag = Tag
  { tagId :: Int64 -- ^ 
  , tagTag :: Text -- ^ 
  , tagPostCount :: Int -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Tag where
  parseJSON = genericParseJSON optionsTag
instance ToJSON Tag where
  toJSON = genericToJSON optionsTag

optionsTag :: Options
optionsTag =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ List.lookup s table
    }
  where
    table =
      [ ("tagId", "id")
      , ("tagTag", "tag")
      , ("tagPostCount", "postCount")
      ]


-- | 
data User = User
  { userId :: Int64 -- ^ 
  , userEmail :: Text -- ^ 
  , userName :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON optionsUser
instance ToJSON User where
  toJSON = genericToJSON optionsUser

optionsUser :: Options
optionsUser =
  defaultOptions
    { omitNothingFields  = True
    , fieldLabelModifier = \s -> fromMaybe ("did not find JSON field name for " ++ show s) $ List.lookup s table
    }
  where
    table =
      [ ("userId", "id")
      , ("userEmail", "email")
      , ("userName", "name")
      ]

