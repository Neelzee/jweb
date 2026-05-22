module Test.Title where

import Prelude
import Effect.Aff (Aff)
import Playwright (Page, assertTitle, goto)

run :: Page -> Aff Unit
run page = do
  goto page "http://localhost:3000"
  assertTitle page "Ønskeliste"
