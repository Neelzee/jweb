module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error, throwException)
import Playwright (close, goto, launch, newPage, title)

main :: Effect Unit
main = launchAff_ do
  browser <- launch
  page <- newPage browser
  goto page "http://localhost:3000"
  t <- title page
  liftEffect $ if t == "Ønskeliste"
    then log "✓ title is 'Ønskeliste'"
    else throwException $ error $ "Expected 'Ønskeliste', got: " <> t
  close browser
