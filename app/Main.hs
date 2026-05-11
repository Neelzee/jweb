{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, forM_, void)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time (addUTCTime, getCurrentTime)
import Database.Persist (Entity (..), delete, deleteWhere, selectList, (!=.), (<.), (==.))
import Database.Persist.Sqlite (ConnectionPool, createSqlitePool, runMigration, runSqlPool)
import Application ()
import Foundation (App (..))
import Model (EntityField (PostDeletedAt, PostImagePostId), Post (..), PostId, migrateAll)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)
import Yesod.Core (toWaiApp)

main :: IO ()
main = do
  uploadDir      <- fromMaybe "/var/lib/jweb/uploads"     <$> lookupEnv "JWEB_UPLOAD_DIR"
  dbPath         <- fromMaybe "/var/lib/jweb/jweb.db"     <$> lookupEnv "JWEB_DB_PATH"
  sessionKeyPath <- fromMaybe "/var/lib/jweb/session.aes" <$> lookupEnv "JWEB_SESSION_KEY"
  staticDir      <- fromMaybe "./static"                  <$> lookupEnv "JWEB_STATIC_DIR"
  port           <- maybe 3000 read                       <$> lookupEnv "JWEB_PORT"
  pool <- runStderrLoggingT $ createSqlitePool (pack dbPath) 5
  runSqlPool (runMigration migrateAll) pool
  waiApp <- toWaiApp (App pool uploadDir sessionKeyPath staticDir)
  startCleanupThread pool
  run port waiApp

startCleanupThread :: ConnectionPool -> IO ()
startCleanupThread pool = void $ forkIO $ do
  cleanup pool
  forever $ do
    threadDelay (24 * 60 * 60 * 1_000_000)
    cleanup pool

cleanup :: ConnectionPool -> IO ()
cleanup pool = do
  now <- getCurrentTime
  let cutoff = addUTCTime (negate $ 30 * 24 * 60 * 60) now
  runStderrLoggingT $ runSqlPool (go cutoff) pool
  where
    go cutoff = do
      old <- selectList [PostDeletedAt !=. Nothing, PostDeletedAt <. Just cutoff] []
      forM_ old $ \(Entity pid _ :: Entity Post) -> do
        deleteWhere [PostImagePostId ==. pid]
        delete pid
