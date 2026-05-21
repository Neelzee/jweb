module Playwright where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)

foreign import data Browser :: Type
foreign import data Page :: Type

foreign import launchImpl :: (Browser -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
foreign import newPageImpl :: Browser -> (Page -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
foreign import gotoImpl :: Page -> String -> (Unit -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
foreign import titleImpl :: Page -> (String -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
foreign import closeImpl :: Browser -> (Unit -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

liftAff :: forall a. ((a -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit) -> Aff a
liftAff f = makeAff \cb -> do
  f (cb <<< Right) (cb <<< Left)
  pure nonCanceler

launch :: Aff Browser
launch = liftAff launchImpl

newPage :: Browser -> Aff Page
newPage browser = liftAff (newPageImpl browser)

goto :: Page -> String -> Aff Unit
goto page url = liftAff (gotoImpl page url)

title :: Page -> Aff String
title page = liftAff (titleImpl page)

close :: Browser -> Aff Unit
close browser = liftAff (closeImpl browser)
