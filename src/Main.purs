module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (toEventTarget) as Document
import Web.DOM.DOMTokenList (add) as DOMTokenList
import Web.DOM.Element (classList, toNode) as Element
import Web.DOM.Node (contains) as Node
import Web.DOM.ParentNode (QuerySelector(..), querySelector) as ParentNode
import Web.Event.Event (EventType(..), target) as Event
import Web.Event.EventTarget (addEventListener, eventListener) as EventTarget
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toDocument, toParentNode) as HTMLDocument
import Web.HTML.Window (document) as Window

main :: Effect Unit
main = do
  win <- HTML.window
  doc <- Window.document win
  mWrapper <- ParentNode.querySelector (ParentNode.QuerySelector "[data-menu-wrapper]") (HTMLDocument.toParentNode doc)
  mMenu    <- ParentNode.querySelector (ParentNode.QuerySelector "#mobile-menu")        (HTMLDocument.toParentNode doc)
  case mWrapper, mMenu of
    Just wrapper, Just menu -> do
      listener <- EventTarget.eventListener \evt ->
        case Event.target evt of
          Nothing -> pure unit
          Just t  -> do
            inside <- Node.contains (Element.toNode wrapper) (unsafeCoerce t)
            unless inside do
              cl <- Element.classList menu
              DOMTokenList.add cl "hidden"
      EventTarget.addEventListener
        (Event.EventType "click") listener false
        (Document.toEventTarget (HTMLDocument.toDocument doc))
    _, _ -> pure unit
