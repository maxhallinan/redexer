module Main where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..))
import Data.Foldable (foldl)
import Data.Traversable (traverse)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Stepper as Stepper
import Web.DOM.Node (Node, childNodes, removeChild)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.HTMLElement (HTMLElement, fromNode)

main :: Effect Unit
main = do
  doc <- map toParentNode $ window >>= document
  nodes <- querySelectorAll (QuerySelector ".explorable") doc >>= toArray
  _ <- traverse removeChildren nodes
  htmlElements <- toHTMLElements nodes
  _ <- traverse initStepper htmlElements
  pure unit
  where
    initStepper element = do
      HA.runHalogenAff do
        body <- HA.awaitBody
        runUI Stepper.component unit element

    toHTMLElements :: Array Node -> Effect (Array HTMLElement)
    toHTMLElements = pure <<< foldl toHTMLElementArray mempty <<< map fromNode
      where
        toHTMLElementArray :: Array HTMLElement -> Maybe HTMLElement -> Array HTMLElement
        toHTMLElementArray els (Just el) = (el:els)
        toHTMLElementArray els Nothing = els

removeChildren :: Node -> Effect Unit
removeChildren node = do
  childNodeArray <- childNodes node >>= toArray
  _ <- traverse (flip removeChild node) childNodeArray
  pure unit
