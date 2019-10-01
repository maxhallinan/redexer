module Component.Stepper (component) where

import Prelude

import Component.Node as Node
import Component.Util as U
import Control.Alt ((<|>))
import Core as Core
import Data.Array (last, mapWithIndex, snoc)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Parse as Parse

type Input =
  { defaultContent :: String
  }

type State =
  { currentNode :: Maybe CurrentNode
  , defaultContent :: String
  , lines :: Array Core.Node
  , reductionOrder :: Array String
  , reductions :: Map String String
  }

type CurrentNode =
  { lineIndex :: Int
  , nodeId :: String
  }

data Action
  = Initialize
  | Applied String
  | ContentChanged { newContent :: String }
  | CurrentNodeChanged (Maybe CurrentNode)

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
  { currentNode: Nothing
  , defaultContent
  , lines: mempty
  , reductionOrder: mempty
  , reductions: mempty
  }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
  HH.div
    [ U.className "stepper" ]
    (mapWithIndex (renderLine state) state.lines)

renderLine :: State -> Int -> Core.Node -> H.ComponentHTML Action ChildSlots Aff
renderLine state@{ currentNode, lines, reductionOrder, reductions } i ast =
  let
      linePos = Node.toLinePos { current: i, total: length lines }

      nodeInput =
        { ast
        , focus: getNodeFocus { lineIndex: i } state
        , lineIndex: i
        , linePos
        , reducedNodeId: Array.index reductionOrder i
        }

      cn = [ { name: "line", cond: true }
           , { name: "last", cond: linePos == Node.Last || linePos == Node.Only }
           ]
  in
  HH.div
    [ U.classNames_ cn ]
    [ HH.slot _nodeSlot i Node.component nodeInput handleMessage ]

getNodeFocus :: { lineIndex :: Int } -> State -> Maybe Node.Focus
getNodeFocus { lineIndex } state =
  getSuccessFocus lineIndex state
  <|> getTodoFocus lineIndex state
  <|> getDoneFocus lineIndex state

getSuccessFocus :: Int -> State -> Maybe Node.Focus
getSuccessFocus lineIndex state = do
  currentNode <- state.currentNode
  if currentNode.lineIndex == lineIndex
    then do
      line <- Array.index state.lines lineIndex
      reducedId <- Array.index state.reductionOrder lineIndex
      reducedNode <- Core.findNode reducedId line
      if Core.isDescendantOf currentNode.nodeId reducedNode
        then pure $ makeFocus reducedId
        else Nothing
    else if currentNode.lineIndex + 1 == lineIndex
      then do
        line <- Array.index state.lines currentNode.lineIndex
        reducedId <- Array.index state.reductionOrder currentNode.lineIndex
        reducedNode <- Core.findNode reducedId line
        successId <- Map.lookup reducedId state.reductions
        if Core.isDescendantOf currentNode.nodeId reducedNode
          then pure $ makeFocus successId
          else Nothing
      else Nothing
  where makeFocus nodeId = { nodeId, highlight: Node.Success }

getTodoFocus :: Int -> State -> Maybe Node.Focus
getTodoFocus lineIndex state = do
  currentNode <- state.currentNode
  line <- Array.index state.lines lineIndex
  node <- Core.findNode currentNode.nodeId line
  applyNode <- Core.closestReduceableAncestor node line
  let isReduced = isJust $ Array.index state.reductionOrder lineIndex
  if lineIndex == currentNode.lineIndex && not isReduced
    then pure $ makeFocus applyNode.id
    else Nothing
  where makeFocus nodeId = { nodeId, highlight: Node.Todo }

getDoneFocus :: Int -> State -> Maybe Node.Focus
getDoneFocus lineIndex state = do
  nodeId <- Array.index state.reductionOrder lineIndex
  pure { nodeId, highlight: Node.Done }

handleMessage :: Node.Message -> Maybe Action
handleMessage = case _ of
  Node.Applied id ->
    Just (Applied id)
  Node.ContentChanged { newContent } ->
    Just (ContentChanged { newContent })
  Node.NodeHoverOn currentNode ->
    Just $ CurrentNodeChanged (Just currentNode)
  Node.NodeHoverOff ->
    Just $ CurrentNodeChanged Nothing

handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots o Aff Unit
handleAction action = case action of
  Initialize ->
    handleInitialize
  Applied id ->
    handleApplied id
  ContentChanged { newContent } ->
    handleContentChanged newContent
  CurrentNodeChanged currentNode ->
    handleCurrentNode currentNode

handleCurrentNode :: forall o. Maybe CurrentNode -> H.HalogenM State Action ChildSlots o Aff Unit
handleCurrentNode currentNode =
  H.modify_ \s -> s { currentNode = currentNode }

handleApplyHoverOff :: forall o. H.HalogenM State Action ChildSlots o Aff Unit
handleApplyHoverOff =
  H.modify_ \s -> s { currentNode = Nothing }

handleApplied :: forall o. String -> H.HalogenM State Action ChildSlots o Aff Unit
handleApplied nodeId = do
  newLine <- reduceLastLine
  maybe return updateState newLine
  where
    return = pure unit
    reduceLastLine = H.gets $ _.lines >>> last >=> Core.betaReduction nodeId
    updateState { node, tree } = do
       H.modify_ \s -> s { lines = snoc s.lines tree
                         , currentNode = Just $ { lineIndex: (length s.lines) - 1, nodeId }
                         , reductionOrder = snoc s.reductionOrder nodeId
                         , reductions = Map.insert nodeId node.id s.reductions
                         }

handleInitialize :: forall o. H.HalogenM State Action ChildSlots o Aff Unit
handleInitialize = do
  defaultContent <- H.gets _.defaultContent
  case Parse.parse defaultContent of
    Left err ->
      pure unit
    Right firstLine -> do
      l <- genIds firstLine
      H.modify_ \s -> s { lines = pure l }
  where genIds = liftEffect <<< Core.genIds

handleContentChanged :: forall o. String -> H.HalogenM State Action ChildSlots o Aff Unit
handleContentChanged newContent = do
  case Parse.parse newContent of
    Left err ->
      pure unit  
    Right newAst -> do
      H.liftEffect $ log (show newAst)
      l <- genIds newAst
      H.modify_ \state -> state{ currentNode = Nothing 
                               , lines = pure l 
                               , reductionOrder = mempty :: Array String
                               , reductions = mempty :: Map String String
                               }
  where genIds = liftEffect <<< Core.genIds
