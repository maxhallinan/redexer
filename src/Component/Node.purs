module Component.Node ( Slot
  , Input
  , LinePos(..)
  , Message(..)
  , Focus
  , Highlight(..)
  , Query(..)
  , component
  , toLinePos
  ) where

import Prelude
import Debug.Trace (spy)

import Component.Util as Util
import Core as Core
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.String as String
import Data.String.CodePoints as CodePoints
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried as EU
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Parse as Parse
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
  , interaction :: Interaction
  , lineIndex :: Int
  , linePos :: LinePos
  , focus :: Maybe Focus
  , reducedNodeId :: Maybe String
  }

data Interaction
  = Disabled
  | Read
  | Write WriteState
derive instance eqInteraction :: Eq Interaction

type WriteState = { pendingContent :: String , parseErr :: Maybe Parse.ParseErr }

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
  | EditorKeyDown (Maybe EditorKey) KeyboardEvent.KeyboardEvent
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
  | EditorOpened { lineIndex :: Int }
  | EditorClosed
  | NewAst { ast :: Core.Node }
  | NodeHoverOn { lineIndex :: Int, nodeId :: String }
  | NodeHoverOff

data Query a
  = Disable { lineIndex :: Int } a
  | Enable a

type Slot = H.Slot Query Message

type ChildSlots = ()

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.mkComponent
  { initialState: initState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                   , handleQuery = handleQuery
                                   , receive = Just <<< InputUpdated
                                   }
  }

initState :: Input -> State
initState i =
  { ast: i.ast
  , interaction: Read
  , lineIndex: i.lineIndex
  , linePos: i.linePos
  , focus: i.focus
  , reducedNodeId: i.reducedNodeId
  }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
  let
    _ = spy "state" state
  in
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
    [ renderReadNode state
    ]

renderEditableLine :: State -> H.ComponentHTML Action ChildSlots Aff
renderEditableLine state =
  case state.interaction of
    Disabled ->
      HH.div
        []
        [ renderReadNode state
        ]
    Read ->
      HH.div
        []
        [ renderReadNode state
        , renderEditBtn
        ]
    Write writeState ->
      let
        errMsg = maybe (HH.text "") renderErrMsg writeState.parseErr
      in
      HH.div
        []
        [ renderWriteNode writeState state
        , errMsg
        ]

renderWriteNode :: WriteState -> State -> H.ComponentHTML Action ChildSlots Aff
renderWriteNode { parseErr } state =
  HH.div
    [ HH.attr (HH.AttrName "contenteditable") "true"
    , Util.className "ast"
    , HE.onKeyDown handleKeyDown
    , HP.id_ $ toEditorId state
    , HP.ref editorRef
    ]
    [ HH.text ""
    ]
  where astText = show state.ast.expr
        errPos = map Parse.errPos parseErr
        handleKeyDown event =
          let
            editorKey = toEditorKey $ KeyboardEvent.key event
          in
          Just $ EditorKeyDown editorKey event

editorRef :: H.RefLabel
editorRef = H.RefLabel "editor"

highlightErrPos :: String -> { column :: Int, line :: Int } -> { before :: String, markText :: String, after :: String }
highlightErrPos astText { column } =
  let
      before = String.take (column - 1) astText

      markText =
        String.codePointAt (column - 1) astText
        # maybe "" CodePoints.singleton

      after = String.drop column astText
  in
  { before, markText, after }

renderErrMsg :: Parse.ParseErr -> H.ComponentHTML Action ChildSlots Aff
renderErrMsg err =
  HH.div
    [ Util.className "error" ]
    [ HH.div [Util.className "error-pointer"] [ HH.text pointer ]
    , HH.div [Util.className "error-msg"] [ HH.text errMsg ]
    ]
  where errMsg = Parse.errMsg err
        col = _.column $ Parse.errPos err
        pointer = Array.replicate (col - 1) '-'
                  # flip Array.snoc '^'
                  # fromCharArray

renderEditBtn :: H.ComponentHTML Action ChildSlots Aff
renderEditBtn =
  HH.button
    [ Util.className "edit"
    , HE.onClick (Just <<< EditBtnClicked)
    ]
    [ HH.text "edit" ]

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
  if state.interaction == Disabled
    then HH.span [ Util.className "disabled" ] [ renderNodeBody state ]
    else
      HH.span
        [ Util.classNames cn
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
  HH.span [ Util.className "variable" ] [ HH.text varName ]

renderLambda :: { param :: String, body :: Core.Node } -> State -> H.ComponentHTML Action ChildSlots Aff
renderLambda { param, body } state =
      HH.span
        [ Util.className "lambda" ]
        [ HH.text "λ"
        , HH.text $ param <> "."
        , renderReadNode (state{ ast = body })
        ]

renderApply :: { fn :: Core.Node, arg :: Core.Node } -> State -> H.ComponentHTML Action ChildSlots Aff
renderApply { fn, arg } state@{ ast, focus, reducedNodeId } =
  HH.span
    [ Util.className "apply"
    ]
    [ HH.text "("
    , renderReadNode (state{ ast = fn })
    , HH.text " "
    , renderReadNode (state{ ast = arg })
    , HH.text ")"
    ]

handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Message Aff (Maybe a)
handleQuery = case _ of
  Disable { lineIndex } next ->
    handleDisableQuery { lineIndex } next
  Enable next ->
    handleEnableQuery next

handleDisableQuery :: forall a. { lineIndex :: Int } -> a -> H.HalogenM State Action ChildSlots Message Aff (Maybe a)
handleDisableQuery { lineIndex } next = do
  currentLine <- H.gets _.lineIndex
  when (currentLine /= lineIndex) setDisabledState
  returnNext
  where setDisabledState = H.modify_ \state -> state{ interaction = Disabled }
        returnNext = pure $ Just next

handleEnableQuery :: forall a. a -> H.HalogenM State Action ChildSlots Message Aff (Maybe a)
handleEnableQuery next = do
  H.modify_ \state -> state{ interaction = Read }
  pure $ Just next

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction = case _ of
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
  EditorKeyDown key event ->
    handleEditorKeyDown key event

handleEditorKeyDown :: Maybe EditorKey -> KeyboardEvent.KeyboardEvent -> H.HalogenM State Action ChildSlots Message Aff Unit
handleEditorKeyDown editorKey event = editorKey # traverse_ \EnterKey -> do
  preventDefault event
  attemptCreateNewAst
  where preventDefault = H.liftEffect <<< Event.preventDefault <<< KeyboardEvent.toEvent

attemptCreateNewAst :: H.HalogenM State Action ChildSlots Message Aff Unit
attemptCreateNewAst = getPendingContent >>= traverse_ \new -> do
  isValid <- isValidContent new
  if isValid
    then attemptParseContent new
    else setEditorReadState
  where getPendingContent = H.gets toPendingContent
        getCurrentContent = H.gets showAst
        showAst = show <<< _.expr <<< _.ast
        setEditorReadState = do
          H.modify_ \state -> state{ interaction = Read }
          raiseEditorClosed
        raiseEditorClosed = H.raise EditorClosed
        getEditorElement = H.getHTMLElementRef editorRef
        isValidContent new = do
          old <- getCurrentContent
          pure $ (new /= "") && (new /= old)
        raiseNewAst ast = H.raise $ NewAst { ast }
        setEditorErrState pendingContent err =
          H.modify_ \state -> state{ interaction = Write { parseErr: Just err, pendingContent } }
        markErrPosInEditor pendingContent err = getEditorElement >>= traverse_ \element ->
          let
            parts = highlightErrPos pendingContent (Parse.errPos err)
            node = HTMLElement.toNode element
          in
          H.liftEffect $ markEditorErrPos parts node
        attemptParseContent new = case Parse.parse new of
          Left err -> do
            setEditorErrState new err
            markErrPosInEditor new err
          Right ast -> do
            raiseNewAst ast
            setEditorReadState

handleEditorContentChanged :: Event -> H.HalogenM State Action ChildSlots Message Aff Unit
handleEditorContentChanged event = do
  getEditorNode # traverse_ \node -> do
    pendingContent <- getPendingContent node
    removeHighlightIfErr pendingContent node
    deleteEditorContentIfEmpty pendingContent node
    updateInteraction pendingContent
  where getEditorNode = Event.target event >>= Node.fromEventTarget
        getPendingContent = H.liftEffect <<< Node.textContent
        updateInteraction content = H.modify_ \state -> state{ interaction = updatePendingContent content state.interaction }
        deleteEditorContentIfEmpty content node =
          if content == ""
            then H.liftEffect $ deleteEditorContent node
            else pure unit
        removeHighlightIfErr content node =
          H.gets (getParseErr <<< _.interaction) >>= traverse_ \_ ->
            H.liftEffect $ setEditorTextContent content node

getParseErr :: Interaction -> Maybe Parse.ParseErr
getParseErr = case _ of
  Disabled ->
    Nothing
  Read ->
    Nothing
  Write editor ->
    editor.parseErr

updatePendingContent :: String -> Interaction -> Interaction
updatePendingContent pendingContent interaction =
  case interaction of
    Disabled ->
      Disabled
    Read ->
      Read
    Write _ ->
      Write { pendingContent, parseErr: Nothing }

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
  raiseEditorOpened
  setEditorWriteState
  getEditorElement >>= traverse_ \element -> do
    setEditorContent element
    bindEditorEvents element
  setEditorFocus
  where setEditorFocus = H.gets toEditorId >>= setFocus
        setEditorWriteState = H.modify_ \state -> state{ interaction = Write { parseErr: Nothing, pendingContent: "" } }
        setTextContent textContent node = H.liftEffect $ Node.setTextContent textContent node
        setEditorContent element = do
          ast <- H.gets _.ast
          setTextContent (show ast.expr) (HTMLElement.toNode element)
        getEditorElement = H.getHTMLElementRef editorRef
        bindEditorEvents element = do
          let eventTarget = HTMLElement.toEventTarget element
          onEvent "blur" handleBlur eventTarget
          onEvent "input" handleInput eventTarget
        onEvent name handler target = void $ H.subscribe $ ES.eventListenerEventSource (EventType name) target handler
        handleBlur _ = Just EditorBlurred
        handleInput = Just <<< EditorContentChanged
        setFocus = H.liftEffect <<< Util.setFocus
        raiseEditorOpened = do
          lineIndex <- H.gets _.lineIndex
          H.raise $ EditorOpened { lineIndex }

handleEditorBlurred :: H.HalogenM State Action ChildSlots Message Aff Unit
handleEditorBlurred = attemptCreateNewAst

stopProp :: MouseEvent -> H.HalogenM State Action ChildSlots Message Aff Unit
stopProp = void <<< liftEffect <<< stopPropagation <<< toEvent

toPendingContent :: State -> Maybe String
toPendingContent state =
  case state.interaction of
    Disabled ->
      Nothing
    Read ->
      Nothing
    Write { pendingContent } ->
      Just pendingContent

toEditorId :: State -> String
toEditorId { ast, lineIndex } = show lineIndex <> ast.id

deleteEditorContent :: Node.Node -> Effect Unit
deleteEditorContent = EU.runEffectFn1 _deleteEditorContent

markEditorErrPos :: { before :: String, markText :: String, after :: String } -> Node.Node -> Effect Unit
markEditorErrPos = EU.runEffectFn2 _markEditorErrPos

setEditorTextContent :: String -> Node.Node -> Effect Unit
setEditorTextContent = EU.runEffectFn2 _setEditorTextContent

foreign import _deleteEditorContent :: EU.EffectFn1 Node.Node Unit
foreign import _markEditorErrPos :: EU.EffectFn2 { before :: String, markText :: String, after :: String } Node.Node Unit
foreign import _setEditorTextContent :: EU.EffectFn2 String Node.Node Unit
