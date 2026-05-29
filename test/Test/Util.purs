module Test.Util where

import Prelude
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

step :: String -> Aff Unit -> Aff Unit
step name action = do
  liftEffect $ log ("  → " <> name)
  action
