module Component.Step
  ( Slot
  , Input
  , StepPos(..)
  , Message(..)
  , Focus
  , Highlight(..)
  , Query(..)
  , component
  , toStepPos
  ) where

import Prelude
import Component.Util as Util
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
import Term (Term(..))
import Term as Term
import Web.DOM.Node as Node
import Web.Event.Event (Event, EventType(..), stopPropagation)
import Web.Event.Event as Event
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

type Input
  = { focus :: Maybe Focus
    , reducedTermId :: Maybe String
    , stepIndex :: Int
    , stepPos :: StepPos
    , term :: Term
    }

type State
  = { focus :: Maybe Focus
    , interaction :: Interaction
    , reducedTermId :: Maybe String
    , stepIndex :: Int
    , stepPos :: StepPos
    , term :: Term
    }

data Interaction
  = Disabled
  | Read
  | Write WriteState

derive instance eqInteraction :: Eq Interaction

type WriteState
  = { pendingContent :: String, parseErr :: Maybe Parse.ParseErr }

type Focus
  = { highlight :: Highlight
    , termId :: String
    }

data Highlight
  = Done
  | Success
  | Todo

data StepPos
  = First
  | Last
  | Middle
  | Only

derive instance eqStepPos :: Eq StepPos

toStepPos :: { current :: Int, total :: Int } -> StepPos
toStepPos { current, total } =
  if total == 1 then
    Only
  else
    if current == 0 then
      First
    else
      if current == total - 1 then
        Last
      else
        Middle

data Action
  = ApplyClicked String MouseEvent
  | EditBtnClicked MouseEvent
  | EditorContentChanged Event
  | EditorBlurred
  | EditorKeyDown (Maybe EditorKey) KeyboardEvent.KeyboardEvent
  | InputUpdated Input
  | TermMouseEnter String MouseEvent
  | TermMouseLeave String MouseEvent

data EditorKey
  = EnterKey

toEditorKey :: String -> Maybe EditorKey
toEditorKey = case _ of
  "Enter" -> Just EnterKey
  _ -> Nothing

data Message
  = Applied String
  | EditorOpened { stepIndex :: Int }
  | EditorClosed
  | NewTerm { term :: Term }
  | TermHoverOn { stepIndex :: Int, termId :: String }
  | TermHoverOff

data Query a
  = Disable { stepIndex :: Int } a
  | Enable a

type Slot
  = H.Slot Query Message

type ChildSlots
  = ()

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.mkComponent
    { initialState: initState
    , render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            , receive = Just <<< InputUpdated
            }
    }

initState :: Input -> State
initState i =
  { focus: i.focus
  , interaction: Read
  , reducedTermId: i.reducedTermId
  , stepIndex: i.stepIndex
  , stepPos: i.stepPos
  , term: i.term
  }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state = case state.stepPos of
  First -> renderEditStep state
  Only -> renderEditStep state
  Middle -> renderReadStep state
  Last -> renderReadStep state

renderReadStep :: State -> H.ComponentHTML Action ChildSlots Aff
renderReadStep state =
  HH.div
    []
    [ renderReadTerm mempty state
    ]

renderEditStep :: State -> H.ComponentHTML Action ChildSlots Aff
renderEditStep state = case state.interaction of
  Disabled ->
    HH.div
      []
      [ renderReadTerm mempty state
      ]
  Read ->
    HH.div
      []
      [ renderReadTerm mempty state
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
  where
  termText = show state.term

  errPos = map Parse.errPos parseErr

  handleKeyDown event =
    let
      editorKey = toEditorKey $ KeyboardEvent.key event
    in
      Just $ EditorKeyDown editorKey event

editorRef :: H.RefLabel
editorRef = H.RefLabel "editor"

highlightErrPos :: String -> { column :: Int, line :: Int } -> { before :: String, markText :: String, after :: String }
highlightErrPos termText { column } =
  let
    before = String.take (column - 1) termText

    markText =
      String.codePointAt (column - 1) termText
        # maybe "" CodePoints.singleton

    after = String.drop column termText
  in
    { before, markText, after }

renderErrMsg :: Parse.ParseErr -> H.ComponentHTML Action ChildSlots Aff
renderErrMsg err =
  HH.div
    [ Util.className "error" ]
    [ HH.div [ Util.className "error-pointer" ] [ HH.text pointer ]
    , HH.div [ Util.className "error-msg" ] [ HH.text errMsg ]
    ]
  where
  errMsg = Parse.errMsg err

  col = _.column $ Parse.errPos err

  pointer =
    Array.replicate (col - 1) '-'
      # flip Array.snoc '^'
      # fromCharArray

renderEditBtn :: H.ComponentHTML Action ChildSlots Aff
renderEditBtn =
  HH.button
    [ Util.className "edit"
    , HE.onClick (Just <<< EditBtnClicked)
    ]
    [ HH.text "edit" ]

renderReadTerm :: Term.Context -> State -> H.ComponentHTML Action ChildSlots Aff
renderReadTerm ctx state =
  let
    uuid = Term.uuid state.term

    handleMouseEnter event = Just $ TermMouseEnter uuid event

    handleMouseLeave event = Just $ TermMouseLeave uuid event

    handleClick event =
      if Term.isRedex state.term then
        Just $ ApplyClicked uuid event
      else
        Nothing

    cn =
      if Term.isRedex state.term && isNothing state.focus then
        nodeClassNames state <> [ "reduceable" ]
      else
        nodeClassNames state
  in
    if state.interaction == Disabled then
      HH.span [ Util.className "disabled" ] [ renderNodeBody ctx state ]
    else
      HH.span
        [ Util.classNames cn
        , HE.onClick handleClick
        , HE.onMouseOver handleMouseEnter
        , HE.onMouseOut handleMouseLeave
        ]
        [ renderNodeBody ctx state ]

renderNodeBody :: Term.Context -> State -> H.ComponentHTML Action ChildSlots Aff
renderNodeBody ctx state = case state.term of
  Var var ann ->
    let
      varName = Term.indexToName ctx { name: var.varName, index: var.index }
    in
      renderVar { varName }
  Fn fn ann ->
    let
      fresh = Term.pickFreshName ctx fn.paramName
    in
      renderFn fresh.ctx { param: fresh.name, body: fn.body } state
  Apply fn arg ann -> renderApply ctx { fn, arg } state

nodeClassNames :: State -> Array String
nodeClassNames { term, focus } = case focus of
  Just { termId, highlight } ->
    if termId == Term.uuid term then
      Array.cons (highlightClassName highlight) base
    else
      base
  Nothing -> base
  where
  base = [ "node" ]

highlightClassName :: Highlight -> String
highlightClassName = case _ of
  Done -> "done"
  Success -> "success"
  Todo -> "todo"

renderVar :: { varName :: String } -> H.ComponentHTML Action ChildSlots Aff
renderVar { varName } = HH.span [ Util.className "variable" ] [ HH.text varName ]

renderFn :: Term.Context -> { param :: String, body :: Term.Term } -> State -> H.ComponentHTML Action ChildSlots Aff
renderFn ctx { param, body } state =
  HH.span
    [ Util.className "lambda" ]
    [ HH.text "λ"
    , HH.text $ param <> "."
    , renderReadTerm ctx (state { term = body })
    ]

renderApply :: Term.Context -> { fn :: Term.Term, arg :: Term.Term } -> State -> H.ComponentHTML Action ChildSlots Aff
renderApply ctx { fn, arg } state@{ term, focus, reducedTermId } =
  HH.span
    [ Util.className "apply"
    ]
    [ HH.text "("
    , renderReadTerm ctx (state { term = fn })
    , HH.text " "
    , renderReadTerm ctx (state { term = arg })
    , HH.text ")"
    ]

handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Message Aff (Maybe a)
handleQuery = case _ of
  Disable { stepIndex } next -> handleDisableQuery { stepIndex } next
  Enable next -> handleEnableQuery next

handleDisableQuery :: forall a. { stepIndex :: Int } -> a -> H.HalogenM State Action ChildSlots Message Aff (Maybe a)
handleDisableQuery { stepIndex } next = do
  currentStep <- H.gets _.stepIndex
  when (currentStep /= stepIndex) setDisabledState
  returnNext
  where
  setDisabledState = H.modify_ \state -> state { interaction = Disabled }

  returnNext = pure $ Just next

handleEnableQuery :: forall a. a -> H.HalogenM State Action ChildSlots Message Aff (Maybe a)
handleEnableQuery next = do
  H.modify_ \state -> state { interaction = Read }
  pure $ Just next

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction = case _ of
  InputUpdated input -> do
    handleInputUpdated input
  ApplyClicked termId event -> do
    handleApplyClicked termId event
  TermMouseEnter termId event -> do
    handleTermMouseEnter termId event
  TermMouseLeave termId event -> handleTermMouseLeave termId event
  EditBtnClicked event -> handleEditBtnClicked event
  EditorBlurred -> handleEditorBlurred
  EditorContentChanged event -> handleEditorContentChanged event
  EditorKeyDown key event -> handleEditorKeyDown key event

handleEditorKeyDown :: Maybe EditorKey -> KeyboardEvent.KeyboardEvent -> H.HalogenM State Action ChildSlots Message Aff Unit
handleEditorKeyDown editorKey event =
  editorKey
    # traverse_ \EnterKey -> do
        preventDefault event
        attemptCreateNewTerm
  where
  preventDefault = H.liftEffect <<< Event.preventDefault <<< KeyboardEvent.toEvent

attemptCreateNewTerm :: H.HalogenM State Action ChildSlots Message Aff Unit
attemptCreateNewTerm =
  getPendingContent
    >>= traverse_ \new -> do
        isValid <- isValidContent new
        if isValid then
          attemptParseContent new
        else
          setEditorReadState
  where
  getPendingContent = H.gets toPendingContent

  getCurrentContent = H.gets showTerm

  showTerm = show <<< _.term

  setEditorReadState = do
    H.modify_ \state -> state { interaction = Read }
    raiseEditorClosed

  raiseEditorClosed = H.raise EditorClosed

  getEditorElement = H.getHTMLElementRef editorRef

  isValidContent new = do
    old <- getCurrentContent
    pure $ (new /= "") && (new /= old)

  raiseNewTerm term = H.raise $ NewTerm { term }

  setEditorErrState pendingContent err =
    H.modify_ \state ->
      state
        { interaction =
          Write
            { parseErr: Just err
            , pendingContent
            }
        }

  markErrPosInEditor pendingContent err =
    getEditorElement
      >>= traverse_ \element ->
          let
            parts = highlightErrPos pendingContent (Parse.errPos err)

            node = HTMLElement.toNode element
          in
            H.liftEffect $ markEditorErrPos parts node

  attemptParseContent new = case Parse.parse new of
    Left err -> do
      setEditorErrState new err
      markErrPosInEditor new err
    Right term -> do
      raiseNewTerm term
      setEditorReadState

handleEditorContentChanged :: Event -> H.HalogenM State Action ChildSlots Message Aff Unit
handleEditorContentChanged event = do
  getEditorNode
    # traverse_ \node -> do
        pendingContent <- getPendingContent node
        removeHighlightIfErr pendingContent node
        deleteEditorContentIfEmpty pendingContent node
        updateInteraction pendingContent
  where
  getEditorNode = Event.target event >>= Node.fromEventTarget

  getPendingContent = H.liftEffect <<< Node.textContent

  updateInteraction content =
    H.modify_ \state ->
      state
        { interaction = updatePendingContent content state.interaction
        }

  deleteEditorContentIfEmpty content node =
    if content == "" then
      H.liftEffect $ deleteEditorContent node
    else
      pure unit

  removeHighlightIfErr content node =
    H.gets (getParseErr <<< _.interaction)
      >>= traverse_ \_ ->
          H.liftEffect $ setEditorTextContent content node

getParseErr :: Interaction -> Maybe Parse.ParseErr
getParseErr = case _ of
  Disabled -> Nothing
  Read -> Nothing
  Write editor -> editor.parseErr

updatePendingContent :: String -> Interaction -> Interaction
updatePendingContent pendingContent interaction = case interaction of
  Disabled -> Disabled
  Read -> Read
  Write _ -> Write { pendingContent, parseErr: Nothing }

handleInputUpdated :: Input -> H.HalogenM State Action ChildSlots Message Aff Unit
handleInputUpdated i =
  H.modify_ \s ->
    s
      { term = i.term
      , stepIndex = i.stepIndex
      , stepPos = i.stepPos
      , focus = i.focus
      , reducedTermId = i.reducedTermId
      }

handleApplyClicked :: String -> MouseEvent -> H.HalogenM State Action ChildSlots Message Aff Unit
handleApplyClicked termId event = do
  stopProp event
  H.raise $ Applied termId

handleTermMouseEnter :: String -> MouseEvent -> H.HalogenM State Action ChildSlots Message Aff Unit
handleTermMouseEnter termId event = do
  stopProp event
  { stepIndex } <- H.get
  H.raise $ TermHoverOn { stepIndex, termId }

handleTermMouseLeave :: String -> MouseEvent -> H.HalogenM State Action ChildSlots Message Aff Unit
handleTermMouseLeave termId event = do
  stopProp event
  H.raise TermHoverOff

handleEditBtnClicked :: MouseEvent -> H.HalogenM State Action ChildSlots Message Aff Unit
handleEditBtnClicked event = do
  stopProp event
  raiseEditorOpened
  setEditorWriteState
  getEditorElement
    >>= traverse_ \element -> do
        setEditorContent element
        bindEditorEvents element
  setEditorFocus
  where
  setEditorFocus = H.gets toEditorId >>= setFocus

  setEditorWriteState =
    H.modify_ \state ->
      state
        { interaction = Write { parseErr: Nothing, pendingContent: "" }
        }

  setTextContent textContent node = H.liftEffect $ Node.setTextContent textContent node

  setEditorContent element = do
    term <- H.gets _.term
    setTextContent (show term) (HTMLElement.toNode element)

  getEditorElement = H.getHTMLElementRef editorRef

  bindEditorEvents element = do
    let
      eventTarget = HTMLElement.toEventTarget element
    onEvent "blur" handleBlur eventTarget
    onEvent "input" handleInput eventTarget

  onEvent name handler target = do
    let
      eventSource = ES.eventListenerEventSource (EventType name) target handler
    void $ H.subscribe eventSource

  handleBlur _ = Just EditorBlurred

  handleInput = Just <<< EditorContentChanged

  setFocus = H.liftEffect <<< Util.setFocus

  raiseEditorOpened = do
    stepIndex <- H.gets _.stepIndex
    H.raise $ EditorOpened { stepIndex }

handleEditorBlurred :: H.HalogenM State Action ChildSlots Message Aff Unit
handleEditorBlurred = attemptCreateNewTerm

stopProp :: MouseEvent -> H.HalogenM State Action ChildSlots Message Aff Unit
stopProp = void <<< liftEffect <<< stopPropagation <<< toEvent

toPendingContent :: State -> Maybe String
toPendingContent state = case state.interaction of
  Disabled -> Nothing
  Read -> Nothing
  Write { pendingContent } -> Just pendingContent

toEditorId :: State -> String
toEditorId { term, stepIndex } = show stepIndex <> Term.uuid term

deleteEditorContent :: Node.Node -> Effect Unit
deleteEditorContent = EU.runEffectFn1 _deleteEditorContent

markEditorErrPos :: { before :: String, markText :: String, after :: String } -> Node.Node -> Effect Unit
markEditorErrPos = EU.runEffectFn2 _markEditorErrPos

setEditorTextContent :: String -> Node.Node -> Effect Unit
setEditorTextContent = EU.runEffectFn2 _setEditorTextContent

foreign import _deleteEditorContent :: EU.EffectFn1 Node.Node Unit

foreign import _markEditorErrPos :: EU.EffectFn2 { before :: String, markText :: String, after :: String } Node.Node Unit

foreign import _setEditorTextContent :: EU.EffectFn2 String Node.Node Unit
