module Main where

import Prelude

import Component.Stepper as Stepper
import Data.Maybe (Maybe(..))
import Data.String.Common (trim)
import Data.Traversable (traverse)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.DOM.Node (Node, childNodes, removeChild, textContent)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (HTMLDocument, toParentNode)
import Web.HTML.HTMLElement (fromNode)

main :: Effect Unit
main = do
  doc <- getDocument
  nodes <- getStepperNodes doc
  initSteppers nodes

getDocument :: Effect HTMLDocument
getDocument = window >>= document

getStepperNodes :: HTMLDocument -> Effect (Array Node)
getStepperNodes =
  toArray
  <=< querySelectorAll (QuerySelector ".stepper")
  <<< toParentNode

initSteppers :: Array Node -> Effect Unit
initSteppers = void <<< traverse init
  where init node =
          case fromNode node of
            Nothing ->
              pure unit
            Just element -> do
              defaultContent <- map trim (textContent node)
              removeChildren node
              HA.runHalogenAff $ runUI Stepper.component { defaultContent } element

removeChildren :: Node -> Effect Unit
removeChildren node = void do
  childNodeArray <- childNodes node >>= toArray
  traverse (flip removeChild node) childNodeArray
