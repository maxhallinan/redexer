module Component.Stepper (component) where

import Prelude
import Component.Step as Step
import Component.Util as U
import Control.Alt ((<|>))
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
import Halogen as H
import Halogen.HTML as HH
import Parse as Parse
import Term (Term)
import Term as Term

type Input
  = { defaultContent :: String
    }

type State
  = { currentNode :: Maybe CurrentNode
    , defaultContent :: String
    , lines :: Array Term
    , parseErr :: Maybe Parse.ParseErr
    , reductionOrder :: Array String
    , reductions :: Map String String
    }

type CurrentNode
  = { lineIndex :: Int
    , nodeId :: String
    }

data Action
  = Initialize
  | Applied String
  | NewAst { ast :: Term }
  | CurrentNodeChanged (Maybe CurrentNode)
  | EditorOpened { lineIndex :: Int }
  | EditorClosed

type ChildSlots
  = ( nodeSlot :: Step.Slot SlotIndex )

type SlotIndex
  = Int

_nodeSlot :: SProxy "nodeSlot"
_nodeSlot = SProxy

component :: forall q o. H.Component HH.HTML q Input o Aff
component =
  H.mkComponent
    { eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
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
  , parseErr: Nothing
  , reductionOrder: mempty
  , reductions: mempty
  }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
  HH.div
    [ U.className "stepper" ]
    (mapWithIndex (renderLine state) state.lines)

renderLine :: State -> Int -> Term -> H.ComponentHTML Action ChildSlots Aff
renderLine state@{ currentNode, lines, reductionOrder, reductions } i ast =
  let
    linePos = Step.toLinePos { current: i, total: length lines }

    nodeInput =
      { ast
      , focus: getNodeFocus { lineIndex: i } state
      , lineIndex: i
      , linePos
      , reducedNodeId: Array.index reductionOrder i
      }

    cn =
      [ { name: "line", cond: true }
      , { name: "last", cond: linePos == Step.Last || linePos == Step.Only }
      ]
  in
    HH.div
      [ U.classNames_ cn ]
      [ HH.slot _nodeSlot i Step.component nodeInput handleMessage ]

getNodeFocus :: { lineIndex :: Int } -> State -> Maybe Step.Focus
getNodeFocus { lineIndex } state =
  getSuccessFocus lineIndex state
    <|> getTodoFocus lineIndex state
    <|> getDoneFocus lineIndex state

getSuccessFocus :: Int -> State -> Maybe Step.Focus
getSuccessFocus lineIndex state = do
  currentNode <- state.currentNode
  if currentNode.lineIndex == lineIndex then do
    line <- Array.index state.lines lineIndex
    reducedId <- Array.index state.reductionOrder lineIndex
    reducedNode <- Term.findTerm reducedId line
    if Term.isDescendantOf currentNode.nodeId reducedNode then
      pure $ makeFocus reducedId
    else
      Nothing
  else
    if currentNode.lineIndex + 1 == lineIndex then do
      line <- Array.index state.lines currentNode.lineIndex
      reducedId <- Array.index state.reductionOrder currentNode.lineIndex
      reducedNode <- Term.findTerm reducedId line
      successId <- Map.lookup reducedId state.reductions
      if Term.isDescendantOf currentNode.nodeId reducedNode then
        pure $ makeFocus successId
      else
        Nothing
    else
      Nothing
  where
  makeFocus nodeId = { nodeId, highlight: Step.Success }

getTodoFocus :: Int -> State -> Maybe Step.Focus
getTodoFocus lineIndex state = do
  currentNode <- state.currentNode
  line <- Array.index state.lines lineIndex
  node <- Term.findTerm currentNode.nodeId line
  applyNode <- Term.closestRedexAncestor (Term.uuid node) line
  let
    isReduced = isJust $ Array.index state.reductionOrder lineIndex
  if lineIndex == currentNode.lineIndex && not isReduced then
    pure $ makeFocus (Term.uuid applyNode)
  else
    Nothing
  where
  makeFocus nodeId = { nodeId, highlight: Step.Todo }

getDoneFocus :: Int -> State -> Maybe Step.Focus
getDoneFocus lineIndex state = do
  nodeId <- Array.index state.reductionOrder lineIndex
  pure { nodeId, highlight: Step.Done }

handleMessage :: Step.Message -> Maybe Action
handleMessage = case _ of
  Step.Applied id -> Just (Applied id)
  Step.EditorOpened { lineIndex } -> Just (EditorOpened { lineIndex })
  Step.EditorClosed -> Just EditorClosed
  Step.NewAst { ast } -> Just (NewAst { ast })
  Step.NodeHoverOn currentNode -> Just $ CurrentNodeChanged (Just currentNode)
  Step.NodeHoverOff -> Just $ CurrentNodeChanged Nothing

handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots o Aff Unit
handleAction action = case action of
  Initialize -> handleInitialize
  Applied id -> handleApplied id
  EditorOpened { lineIndex } -> handleEditorOpened lineIndex
  EditorClosed -> handleEditorClosed
  NewAst { ast } -> handleNewAst ast
  CurrentNodeChanged currentNode -> handleCurrentNode currentNode

handleEditorOpened :: forall o. Int -> H.HalogenM State Action ChildSlots o Aff Unit
handleEditorOpened lineIndex = void $ H.queryAll _nodeSlot (H.tell $ Step.Disable { lineIndex })

handleEditorClosed :: forall o. H.HalogenM State Action ChildSlots o Aff Unit
handleEditorClosed = void $ H.queryAll _nodeSlot (H.tell Step.Enable)

handleCurrentNode :: forall o. Maybe CurrentNode -> H.HalogenM State Action ChildSlots o Aff Unit
handleCurrentNode currentNode = H.modify_ \s -> s { currentNode = currentNode }

handleApplyHoverOff :: forall o. H.HalogenM State Action ChildSlots o Aff Unit
handleApplyHoverOff = H.modify_ \s -> s { currentNode = Nothing }

handleApplied :: forall o. String -> H.HalogenM State Action ChildSlots o Aff Unit
handleApplied nodeId = do
  newLine <- reduceLastLine
  maybe return updateState newLine
  where
  return = pure unit

  reduceLastLine = H.gets $ _.lines >>> last >=> Term.reduce nodeId

  updateState { step, term } = do
    H.modify_ \s ->
      s
        { lines = snoc s.lines term
        , currentNode = Just $ { lineIndex: (length s.lines) - 1, nodeId }
        , reductionOrder = snoc s.reductionOrder nodeId
        , reductions = Map.insert nodeId (Term.uuid step) s.reductions
        }

handleInitialize :: forall o. H.HalogenM State Action ChildSlots o Aff Unit
handleInitialize = do
  defaultContent <- H.gets _.defaultContent
  case Parse.parse defaultContent of
    Left err -> pure unit
    Right firstLine -> do
      l <- genIds firstLine
      H.modify_ \s -> s { lines = pure l }
  where
  genIds = liftEffect <<< Term.genIds

handleNewAst :: forall o. Term -> H.HalogenM State Action ChildSlots o Aff Unit
handleNewAst ast = do
  l <- genIds ast
  H.modify_ \state ->
    state
      { currentNode = Nothing
      , lines = pure l
      , parseErr = Nothing
      , reductionOrder = mempty :: Array String
      , reductions = mempty :: Map String String
      }
  where
  genIds = liftEffect <<< Term.genIds
