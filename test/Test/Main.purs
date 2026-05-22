module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Playwright (close, launch, newPage, setDefaultTimeout)
import Test.NewTagSelected as NewTagSelected
import Test.Title as Title

runTest :: String -> Aff Unit -> Aff Unit
runTest name test = do
  liftEffect $ log ("▶ " <> name)
  test
  liftEffect $ log ("✓ " <> name)

main :: Effect Unit
main = launchAff_ $
  bracket launch close \browser -> do
    page <- newPage browser
    liftEffect $ setDefaultTimeout page 10000
    runTest "title" (Title.run page)
    runTest "new tag is selected after creation" (NewTagSelected.run page)
