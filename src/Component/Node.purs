module Component.Node
  ( Slot
  , Input
  , LinePos(..)
  , Message(..)
  , Query
  , component
  , toLinePos
  ) where

import Prelude

import Component.Util as U
import Core as Core
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

import Debug.Trace (spy)

type Input =
  { ast :: Core.Node
  , highlightedNode :: Maybe String
  , lineIndex :: Int
  , linePos :: LinePos
  , reducedNodeId :: Maybe String
  }

type State =
  { ast :: Core.Node
  , highlightedNode :: Maybe String
  , isHoverEnabled :: Boolean
  , lineIndex :: Int
  , linePos :: LinePos
  , reducedNodeId :: Maybe String
  }

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
  | ApplyMouseEnter String
  | ApplyMouseLeave String
  | InputUpdated Input

data Message 
  = Applied String
  | ApplyHoverOn { lineIndex :: Int, nodeId :: String }
  | ApplyHoverOff

data Query a

type Slot = H.Slot Query Message

type ChildSlots = ()

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.mkComponent
  { initialState: initState -- Input -> State
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                   , receive = Just <<< InputUpdated
                                   }
  }

initState :: Input -> State
initState i = 
  { ast: i.ast
  , highlightedNode: i.highlightedNode
  , isHoverEnabled: false
  , lineIndex: i.lineIndex
  , linePos: i.linePos
  , reducedNodeId: i.reducedNodeId
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
    isHighlighted = maybe false (_ == state.ast.id) state.highlightedNode

    cn = [ {name: "highlighted", cond: isHighlighted}
         ]

    n = case state.ast.expr of
          Core.Var varName ->
            renderVar { varName }
          Core.Lambda param body ->
            renderLambda { param, body } state
          Core.Apply fn arg ->
            renderApply { fn, arg } state
  in
  HH.span 
    [ U.classNames_ cn
    ]
    [n]

renderVar :: { varName :: String } -> H.ComponentHTML Action ChildSlots Aff
renderVar { varName } =
  HH.span [ U.className "variable" ] [ HH.text varName ]

renderLambda :: { param :: String, body :: Core.Node } -> State -> H.ComponentHTML Action ChildSlots Aff
renderLambda { param, body } state =
      HH.span
        [ U.className "lambda" ]
        [ HH.text "λ"
        , HH.text $ param <> "."
        , renderNode $ state { ast = body }
        ]

renderApply :: { fn :: Core.Node, arg :: Core.Node } -> State -> H.ComponentHTML Action ChildSlots Aff
renderApply { fn, arg } s@{ ast, highlightedNode, reducedNodeId } =
  let
      isClickable = Core.isReduceable fn

      isReducedNode = maybe false (_ == ast.id) reducedNodeId

      isHighlighted = maybe false (_ == ast.id) highlightedNode

      cn = [ {name: "applicable", cond: isClickable}
           , {name: "apply", cond: true}
           , {name: "clicked", cond: isReducedNode}
           , {name: "highlighted", cond: isHighlighted}
           ]

      handleClick event =
        if isClickable
          then Just $ ApplyClicked ast.id event
          else Nothing

      handleMouseEnter _ = 
        case s.linePos of
          Only ->
            Just $ ApplyMouseEnter ast.id
          Last ->
            Just $ ApplyMouseEnter ast.id
          _ ->
            case reducedNodeId of
              Just id ->
                if id == ast.id
                  then Just (ApplyMouseEnter id)
                  else Nothing
              Nothing -> 
                Nothing

      handleMouseLeave _ = 
        case s.linePos of
          Only ->
            Just $ ApplyMouseLeave ast.id
          Last ->
            Just $ ApplyMouseLeave ast.id
          _ ->
            case reducedNodeId of
              Just id ->
                if id == ast.id
                  then Just (ApplyMouseLeave id)
                  else Nothing
              Nothing -> 
                Nothing
  in
  HH.span
    [ U.classNames_ cn
    , HE.onClick handleClick
    , HE.onMouseEnter handleMouseEnter
    , HE.onMouseLeave handleMouseLeave
    ]
    [ HH.text "("
    , renderNode $ s { ast = fn }
    , HH.text " "
    , renderNode $ s { ast = arg }
    , HH.text ")"
    ]

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction = case _ of
  InputUpdated input -> do
    lastState <- H.get
    H.modify_ \s -> s { ast = input.ast
                      , highlightedNode = input.highlightedNode
                      , lineIndex = input.lineIndex
                      , linePos = input.linePos
                      , reducedNodeId = input.reducedNodeId
                      , isHoverEnabled = lastState.ast.id == input.ast.id && lastState.isHoverEnabled
                      }
  ApplyClicked id event -> do
    stopProp event
    H.raise $ Applied id
  ApplyMouseEnter nodeId -> do
    isHoverEnabled <- H.gets _.isHoverEnabled
    if isHoverEnabled
      then do
        { lineIndex } <- H.get
        H.raise $ ApplyHoverOn { lineIndex, nodeId }
      else do
         H.modify_ \s -> s { isHoverEnabled = true }
  ApplyMouseLeave id -> do
    isHoverEnabled <- H.gets _.isHoverEnabled
    if isHoverEnabled
      then H.raise $ ApplyHoverOff
      else H.modify_ \s -> s { isHoverEnabled = true }
  where stopProp = void <<< liftEffect <<< stopPropagation <<< toEvent
