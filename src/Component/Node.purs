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
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Input =
  { ast :: Core.Node
  , lineIndex :: Int
  , linePos :: LinePos
  , focus :: Maybe Focus
  , reducedNodeId :: Maybe String
  }

type State =
  { ast :: Core.Node
  , lineIndex :: Int
  , linePos :: LinePos
  , focus :: Maybe Focus
  , reducedNodeId :: Maybe String
  }

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
  | NodeMouseEnter String MouseEvent
  | NodeMouseLeave String MouseEvent
  | InputUpdated Input

data Message 
  = Applied String
  | NodeHoverOn { lineIndex :: Int, nodeId :: String }
  | NodeHoverOff

data Query a

type Slot = H.Slot Query Message

type ChildSlots = ()

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.mkComponent
  { initialState: identity -- Input -> State
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                   , receive = Just <<< InputUpdated
                                   }
  }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
  HH.div
    []
    [ renderNode state
    , renderEditBtn state
    ]

renderEditBtn :: State -> H.ComponentHTML Action ChildSlots Aff
renderEditBtn { linePos } =
  case linePos of
    First ->
      editBtn
    Last ->
      placeholder
    Middle ->
      placeholder
    Only ->
      editBtn
  where editBtn = HH.span [ U.className "edit" ] [ HH.text "edit" ]
        placeholder = HH.text ""

renderNode :: State -> H.ComponentHTML Action ChildSlots Aff
renderNode state =
  let
    handleMouseEnter event = 
      Just $ NodeMouseEnter state.ast.id event

    handleMouseLeave event = 
      Just $ NodeMouseLeave state.ast.id event

    handleClick event =
      if Core.isReduceable state.ast
        then Just $ ApplyClicked state.ast.id event
        else Nothing
  in
  HH.span 
    [ U.classNames $ nodeClassNames state
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

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction = case _ of
  InputUpdated input ->
    H.put input
  ApplyClicked id event -> do
    stopProp event
    H.raise $ Applied id
  NodeMouseEnter nodeId event -> do
    stopProp event
    { lineIndex } <- H.get
    H.raise $ NodeHoverOn { lineIndex, nodeId }
  NodeMouseLeave nodeId event -> do
    stopProp event
    H.raise $ NodeHoverOff
  where stopProp = void <<< liftEffect <<< stopPropagation <<< toEvent
