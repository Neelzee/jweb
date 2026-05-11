module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Database.Persist.Sqlite (createSqlitePool, runMigration, runSqlPool)
import Application ()
import Foundation (App (..))
import Model (migrateAll)
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
  run port waiApp
