module Component.Stepper (component) where

import Prelude

import Component.Node as Node
import Component.Util as U
import Core as Core
import Data.Array (last, mapWithIndex, snoc)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Parse as Parse

import Debug.Trace (spy)

type Input =
  { defaultContent :: String
  }

type State =
  { currentApplied :: Maybe { lineIndex :: Int, nodeId :: String }
  , defaultContent :: String
  , lines :: Array Core.Node
  , reducedNodes :: Array String
  , reductions :: Map String String
  }

data Action
  = Initialize
  | Applied String
  | ApplyHoverOn { lineIndex :: Int, nodeId :: String }
  | ApplyHoverOff

type ChildSlots = ( nodeSlot :: Node.Slot SlotIndex )

type SlotIndex = Int

_nodeSlot :: SProxy "nodeSlot"
_nodeSlot = SProxy

component :: forall q o. H.Component HH.HTML q Input o Aff
component =
  H.mkComponent
    { eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    , initialState: initState
    , render
    }

initState :: Input -> State
initState { defaultContent } =
  { currentApplied: Nothing
  , defaultContent
  , lines: mempty
  , reducedNodes: mempty
  , reductions: mempty
  }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
  HH.div
    [ U.className "stepper" ]
    (mapWithIndex (renderLine state) state.lines)

renderLine :: State -> Int -> Core.Node -> H.ComponentHTML Action ChildSlots Aff
renderLine { currentApplied, lines, reducedNodes, reductions } i ast =
  let
      linePos = Node.toLinePos { current: i, total: length lines }

      nodeInput =
        { ast
        , highlightedNode: getHighlightedNodeId { currentLineIndex: i, reductions } currentApplied
        , reducedNodeId: Array.index reducedNodes i
        , lineIndex: i
        , linePos
        }

      isHovering {lineIndex} = 
        lineIndex == i || lineIndex + 1 == i

      cn = [ { name: "line", cond: true }
           , { name: "last", cond: linePos == Node.Last || linePos == Node.Only }
           , { name: "hovering", cond: maybe false isHovering currentApplied }
           ]
  in
  HH.div
    [ U.classNames_ cn ]
    [ HH.slot _nodeSlot i Node.component nodeInput handleMessage ]

getHighlightedNodeId
  :: { currentLineIndex :: Int, reductions :: Map String String } 
  -> Maybe { lineIndex :: Int , nodeId :: String } 
  -> Maybe String
getHighlightedNodeId { currentLineIndex, reductions } highlightedNode = highlightedNode >>= \{ lineIndex, nodeId } ->
  if currentLineIndex == lineIndex
    then map (const nodeId) (Map.lookup nodeId reductions)
    else if (currentLineIndex - 1) == lineIndex
      then Map.lookup nodeId reductions
      else Nothing

handleMessage :: Node.Message -> Maybe Action
handleMessage = case _ of
  Node.Applied id ->
    Just (Applied id)
  Node.ApplyHoverOn nodeLoc ->
    Just (ApplyHoverOn nodeLoc)
  Node.ApplyHoverOff ->
    Just ApplyHoverOff

handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots o Aff Unit
handleAction action = case action of
  Initialize ->
    handleInitialize
  Applied id ->
    handleApplied id
  ApplyHoverOn nodeLoc ->
    handleApplyHoverOn nodeLoc
  ApplyHoverOff ->
    handleApplyHoverOff

handleApplyHoverOn :: forall o. { lineIndex :: Int, nodeId :: String } -> H.HalogenM State Action ChildSlots o Aff Unit
handleApplyHoverOn nodeLoc =
  H.modify_ \s -> s { currentApplied = Just nodeLoc }

handleApplyHoverOff :: forall o. H.HalogenM State Action ChildSlots o Aff Unit
handleApplyHoverOff =
  H.modify_ \s -> s { currentApplied = Nothing }

handleApplied :: forall o. String -> H.HalogenM State Action ChildSlots o Aff Unit
handleApplied nodeId = do
  newLine <- reduceLastLine
  maybe return updateState newLine
  where
    return = pure unit
    reduceLastLine = H.gets $ _.lines >>> last >=> Core.betaReduction nodeId
    updateState { node, tree } = do
       H.modify_ \s -> s { lines = snoc s.lines tree
                         , currentApplied = Just $ { lineIndex: (length s.lines) - 1, nodeId }
                         , reducedNodes = snoc s.reducedNodes nodeId
                         , reductions = Map.insert nodeId node.id s.reductions
                         }

handleInitialize :: forall o. H.HalogenM State Action ChildSlots o Aff Unit
handleInitialize = do
  defaultContent <- H.gets _.defaultContent
  case Parse.parse defaultContent of
    Left err -> do
      pure unit
    Right firstLine -> do
      l <- genIds firstLine
      H.modify_ \s -> s { lines = pure l }
  where genIds = liftEffect <<< Core.genIds
