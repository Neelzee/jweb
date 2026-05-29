module Playwright where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)

foreign import data Browser :: Type
foreign import data Page :: Type

foreign import data Locator :: Type

foreign import launchImpl :: (Browser -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
foreign import newPageImpl :: Browser -> (Page -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
foreign import gotoImpl :: Page -> String -> (Unit -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
foreign import titleImpl :: Page -> (String -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
foreign import closeImpl :: Browser -> (Unit -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
foreign import assertTitleImpl :: Page -> String -> (Unit -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
foreign import setDefaultTimeoutImpl :: Page -> Int -> Effect Unit
foreign import locatorImpl :: Page -> String -> Locator
foreign import clickImpl :: Locator -> (Unit -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
foreign import fillImpl :: Locator -> String -> (Unit -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
foreign import assertCheckedImpl :: Locator -> (Unit -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit

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

assertTitle :: Page -> String -> Aff Unit
assertTitle page expected = liftAff (assertTitleImpl page expected)

setDefaultTimeout :: Page -> Int -> Effect Unit
setDefaultTimeout = setDefaultTimeoutImpl

locator :: Page -> String -> Locator
locator = locatorImpl

click :: Locator -> Aff Unit
click l = liftAff (clickImpl l)

fill :: Locator -> String -> Aff Unit
fill l text = liftAff (fillImpl l text)

assertChecked :: Locator -> Aff Unit
assertChecked l = liftAff (assertCheckedImpl l)
