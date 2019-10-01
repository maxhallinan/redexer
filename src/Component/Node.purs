module Component.Node
  ( Slot
  , Input
  , LinePos(..)
  , Message(..)
  , Focus
  , Highlight(..)
  , Query
  , component
  , toLinePos
  ) where

import Prelude

import Component.Util as U
import Core as Core
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), isNothing)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Component.Util as Util
import Web.DOM.Node (textContent)
import Web.DOM.Node as Node
import Web.Event.Event (Event, EventType(..), stopPropagation)
import Web.Event.Event as Event
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

type Input =
  { ast :: Core.Node
  , lineIndex :: Int
  , linePos :: LinePos
  , focus :: Maybe Focus
  , reducedNodeId :: Maybe String
  }

type State =
  { ast :: Core.Node
  , editorState :: EditorState
  , lineIndex :: Int
  , linePos :: LinePos
  , focus :: Maybe Focus
  , reducedNodeId :: Maybe String
  }

data EditorState
  = Read
  | Write { pendingContent :: String }
derive instance eqEditorState :: Eq EditorState

type Focus =
  { highlight :: Highlight
  , nodeId :: String
  }

data Highlight
  = Done
  | Success
  | Todo

data LinePos = First | Last | Middle | Only
derive instance eqLinePos :: Eq LinePos

toLinePos :: { current :: Int, total :: Int } -> LinePos
toLinePos { current, total } =
  if total == 1
  then Only
  else if current == 0
    then First
    else if current == total - 1
      then Last
      else Middle

data Action
  = ApplyClicked String MouseEvent
  | EditBtnClicked MouseEvent
  | EditorContentChanged Event
  | EditorBlurred
  | EditorKeyDown (Maybe EditorKey)
  | Init
  | InputUpdated Input
  | NodeMouseEnter String MouseEvent
  | NodeMouseLeave String MouseEvent

data EditorKey
  = EnterKey

toEditorKey :: String -> Maybe EditorKey
toEditorKey = case _ of
  "Enter" ->
    Just EnterKey
  _ ->
    Nothing

data Message
  = Applied String
  | ContentChanged { newContent :: String }
  | NodeHoverOn { lineIndex :: Int, nodeId :: String }
  | NodeHoverOff

data Query a

type Slot = H.Slot Query Message

type ChildSlots = ()

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.mkComponent
  { initialState: initState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                   , initialize = Just Init
                                   , receive = Just <<< InputUpdated
                                   }
  }

initState :: Input -> State
initState i =
  { ast: i.ast
  , editorState: Read
  , lineIndex: i.lineIndex
  , linePos: i.linePos
  , focus: i.focus
  , reducedNodeId: i.reducedNodeId
  }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
  case state.linePos of
    First ->
      renderEditableLine state
    Only ->
      renderEditableLine state
    Middle ->
      renderReadOnlyLine state
    Last ->
      renderReadOnlyLine state

renderReadOnlyLine :: State -> H.ComponentHTML Action ChildSlots Aff
renderReadOnlyLine state =
  HH.div
    []
    [ renderAst state
    ]

renderEditableLine :: State -> H.ComponentHTML Action ChildSlots Aff
renderEditableLine state =
  HH.div
    []
    [ renderAst state
    , renderEditBtn
    ]

renderAst :: State -> H.ComponentHTML Action ChildSlots Aff
renderAst state =
  let
      contenteditable =
        if state.editorState == Read
          then "false"
          else "true"

      handleKeyDown = Just <<< EditorKeyDown <<< toEditorKey <<< KeyboardEvent.key
  in
  HH.div
    [ HH.attr (HH.AttrName "contenteditable") contenteditable
    , U.className "ast"
    , HE.onKeyDown handleKeyDown
    , HP.id_ $ getAstId state
    , HP.ref editorRef
    ]
    [ renderNode state
    ]

editorRef :: H.RefLabel
editorRef = H.RefLabel "editor"

renderNode :: State -> H.ComponentHTML Action ChildSlots Aff
renderNode state =
  case state.editorState of
    Read ->
      renderReadNode state
    Write _ ->
      renderWriteNode (show state.ast.expr) state

renderWriteNode :: String -> State -> H.ComponentHTML Action ChildSlots Aff
renderWriteNode pendingContent state =
  HH.span
    []
    [ HH.text pendingContent
    ]

renderReadNode :: State -> H.ComponentHTML Action ChildSlots Aff
renderReadNode state =
  let
    handleMouseEnter event =
      Just $ NodeMouseEnter state.ast.id event

    handleMouseLeave event =
      Just $ NodeMouseLeave state.ast.id event

    handleClick event =
      if Core.isReduceable state.ast
        then Just $ ApplyClicked state.ast.id event
        else Nothing

    cn =
      if Core.isReduceable state.ast && isNothing state.focus
        then nodeClassNames state <> ["reduceable"]
        else nodeClassNames state
  in
  HH.span
    [ U.classNames cn
    , HE.onClick handleClick
    , HE.onMouseOver handleMouseEnter
    , HE.onMouseOut handleMouseLeave
    ]
    [ renderNodeBody state ]

renderNodeBody :: State -> H.ComponentHTML Action ChildSlots Aff
renderNodeBody state =
  case state.ast.expr of
    Core.Var varName ->
      renderVar { varName }
    Core.Lambda param body ->
      renderLambda { param, body } state
    Core.Apply fn arg ->
      renderApply { fn, arg } state

nodeClassNames :: State -> Array String
nodeClassNames { ast, focus } =
  case focus of
    Just { nodeId, highlight } ->
      if nodeId == ast.id
        then Array.cons (highlightClassName highlight) base
        else base
    Nothing ->
      base
  where base = ["node"]

highlightClassName :: Highlight -> String
highlightClassName = case _ of
  Done ->
    "done"
  Success ->
    "success"
  Todo ->
    "todo"

renderVar :: { varName :: String } -> H.ComponentHTML Action ChildSlots Aff
renderVar { varName } =
  HH.span [ U.className "variable" ] [ HH.text varName ]

renderLambda :: { param :: String, body :: Core.Node } -> State -> H.ComponentHTML Action ChildSlots Aff
renderLambda { param, body } state =
      HH.span
        [ U.className "lambda" ]
        [ HH.text "λ"
        , HH.text $ param <> "."
        , renderNode (state{ ast = body })
        ]

renderApply :: { fn :: Core.Node, arg :: Core.Node } -> State -> H.ComponentHTML Action ChildSlots Aff
renderApply { fn, arg } state@{ ast, focus, reducedNodeId } =
  HH.span
    [ U.className "apply"
    ]
    [ HH.text "("
    , renderNode (state{ ast = fn })
    , HH.text " "
    , renderNode (state{ ast = arg })
    , HH.text ")"
    ]

renderEditBtn :: H.ComponentHTML Action ChildSlots Aff
renderEditBtn =
  HH.button
    [ U.className "edit"
    , HE.onClick (Just <<< EditBtnClicked)
    ]
    [ HH.text "edit" ]

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction = case _ of
  Init ->
    pure unit
  InputUpdated input -> do
    handleInputUpdated input
  ApplyClicked nodeId event -> do
    handleApplyClicked nodeId event
  NodeMouseEnter nodeId event -> do
    handleNodeMouseEnter nodeId event
  NodeMouseLeave nodeId event ->
    handleNodeMouseLeave nodeId event
  EditBtnClicked event ->
    handleEditBtnClicked event
  EditorBlurred ->
    handleEditorBlurred
  EditorContentChanged event ->
    handleEditorContentChanged event
  EditorKeyDown key ->
    handleEditorKeyDown key

handleEditorKeyDown :: Maybe EditorKey -> H.HalogenM State Action ChildSlots Message Aff Unit
handleEditorKeyDown editorKey =
  case editorKey of
    Just EnterKey ->
      handleEditorBlurred
    Nothing ->
      pure unit

handleEditorContentChanged :: Event -> H.HalogenM State Action ChildSlots Message Aff Unit
handleEditorContentChanged event = do
  let target = Event.target event >>= Node.fromEventTarget
  case target of
    Just t -> do
      pendingContent <- H.liftEffect $ textContent t
      H.modify_ \state -> state{ editorState = updatePendingContent pendingContent state.editorState }
    Nothing -> do
      pure unit

updatePendingContent :: String -> EditorState -> EditorState
updatePendingContent pendingContent editorState =
  case editorState of
    Read ->
      Read
    Write _ ->
      Write { pendingContent }

handleInputUpdated :: Input -> H.HalogenM State Action ChildSlots Message Aff Unit
handleInputUpdated i =
  H.modify_ \s -> s{ ast = i.ast
                    , lineIndex = i.lineIndex
                    , linePos = i.linePos
                    , focus = i.focus
                    , reducedNodeId = i.reducedNodeId
                    }

handleApplyClicked :: String -> MouseEvent -> H.HalogenM State Action ChildSlots Message Aff Unit
handleApplyClicked nodeId event = do
  stopProp event
  H.raise $ Applied nodeId

handleNodeMouseEnter :: String -> MouseEvent -> H.HalogenM State Action ChildSlots Message Aff Unit
handleNodeMouseEnter nodeId event = do
  stopProp event
  { lineIndex } <- H.get
  H.raise $ NodeHoverOn { lineIndex, nodeId }

handleNodeMouseLeave :: String -> MouseEvent -> H.HalogenM State Action ChildSlots Message Aff Unit
handleNodeMouseLeave nodeId event = do
  stopProp event
  H.raise NodeHoverOff

handleEditBtnClicked :: MouseEvent -> H.HalogenM State Action ChildSlots Message Aff Unit
handleEditBtnClicked event = do
  stopProp event
  astId <- H.gets getAstId
  setFocus astId
  H.modify_ \state -> state{ editorState = Write { pendingContent: "" } }
  H.getHTMLElementRef editorRef >>= traverse_ \el -> do
    let eventTarget = HTMLElement.toEventTarget el
    let onEvent eventName handler = void $ H.subscribe $ ES.eventListenerEventSource (EventType eventName) eventTarget handler
    onEvent "blur" (const $ Just EditorBlurred)
    onEvent "input" (Just <<< EditorContentChanged)

handleEditorBlurred :: H.HalogenM State Action ChildSlots Message Aff Unit
handleEditorBlurred = do
  old <- H.gets showAst
  pendingContent >>= traverse_ \new -> do
    if (new /= "") && (new /= old)
      then H.raise $ ContentChanged { newContent: new }
      else pure unit
  H.modify_ \state -> state{ editorState = Read }
  where pendingContent = H.gets getPendingContent
        showAst = show <<< _.expr <<< _.ast

stopProp :: MouseEvent -> H.HalogenM State Action ChildSlots Message Aff Unit
stopProp = void <<< liftEffect <<< stopPropagation <<< toEvent

setFocus :: String -> H.HalogenM State Action ChildSlots Message Aff Unit
setFocus = H.liftEffect <<< Util.setFocus

getPendingContent :: State -> Maybe String
getPendingContent state =
  case state.editorState of
    Read ->
      Nothing
    Write { pendingContent } ->
      Just pendingContent

getAstId :: State -> String
getAstId { ast, lineIndex } = show lineIndex <> ast.id

toggleEditorState :: String -> EditorState -> EditorState
toggleEditorState pendingContent = case _ of
  Read ->
    Write { pendingContent }
  Write _ ->
    Read
