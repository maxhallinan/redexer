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

type Input =
  { ast :: Core.Node
  , linePos :: LinePos
  , reducedNodeId :: Maybe String
  }

type State =
  { ast :: Core.Node
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

data Action = ApplyClicked String MouseEvent | InputUpdated Input

data Message = Applied String

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
renderNode state = do
  case state.ast.expr of
    Core.Var varName ->
      renderVar { varName }
    Core.Lambda param body ->
      renderLambda { param, body } state
    Core.Apply fn arg ->
      renderApply { fn, arg } state

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
renderApply { fn, arg } s@{ ast, reducedNodeId } =
  let
      isClickable = Core.isReduceable fn

      isReducedNode = maybe false (_ == ast.id) reducedNodeId

      cn = [ {name: "applicable", cond: isClickable}
           , {name: "apply", cond: true}
           , {name: "clicked", cond: isReducedNode}
           ]

      handleClick event =
        if isClickable
          then Just $ ApplyClicked ast.id event
          else Nothing
  in
  HH.span
    [ U.classNames_ cn
    , HE.onClick handleClick
    ]
    [ HH.text "("
    , renderNode $ s { ast = fn }
    , HH.text " "
    , renderNode $ s { ast = arg }
    , HH.text ")"
    ]

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction = case _ of
  InputUpdated input ->
    H.put input
  ApplyClicked id event -> do
    stopProp event
    H.raise $ Applied id
  where stopProp = void <<< liftEffect <<< stopPropagation <<< toEvent
