module Test.NewTagSelected where

import Prelude
import Effect.Aff (Aff)
import Playwright (Page, assertChecked, click, fill, goto, locator)
import Test.Util (step)

newTagName :: String
newTagName = "playwright-test-new-tag"

run :: Page -> Aff Unit
run page = do
  step "login via test session" $
    goto page "http://localhost:3000/auth/test-session"
  step "navigate to /post/new" $
    goto page "http://localhost:3000/post/new"
  step "open tag editor" $
    click (locator page "button:has-text('Rediger kategorier')")
  step "click new tag" $
    click (locator page "button:has-text('+ Ny kategori')")
  step "fill tag name" $
    fill (locator page "input[name='tag']") newTagName
  step "submit tag" $
    click (locator page "button:has-text('Opprett')")
  step "go back to select view" $
    click (locator page "button:has-text('Tilbake')")
  step "assert new tag is checked" $
    assertChecked (locator page ("label:has-text('" <> newTagName <> "') input[type='checkbox']"))
