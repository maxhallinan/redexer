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
import Halogen.HTML.Properties as HP
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Slot = H.Slot Query Message

type Input =
  { ast :: Core.Node
  , clickedNodeId :: Maybe String
  , linePos :: LinePos
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

type State =
  { ast :: Core.Node
  , clickedNodeId :: Maybe String
  , linePos :: LinePos
  }

data Message = Applied String

data Query a

type ChildSlots = ()

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.mkComponent
  { initialState: initState
  , render: renderNode
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                   , receive = Just <<< InputUpdated
                                   }
  }

initState :: Input -> State
initState = identity

renderNode :: State -> H.ComponentHTML Action ChildSlots Aff
renderNode state =
  let
    edit = if state.linePos == First || state.linePos == Only
           then HH.span [ HP.class_ $ HH.ClassName "edit" ] [ HH.text "edit" ]
           else HH.text ""
  in
  HH.div
    []
    [ render state
    , edit
    ]

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state = do
  case state.ast.expr of
    Core.Var varName ->
      HH.text varName
    Core.Lambda param body ->
      HH.span
        []
        [ HH.span [ HP.classes [ HH.ClassName "lambda" ] ] [ HH.text "λ" ]
        , HH.text $ param <> "."
        , render $ state { ast = body }
        ]
    Core.Apply e1 e2 ->
      let
          cn = [ {name: "applicable", cond: isApplicable e1}
               , {name: "apply", cond: true}
               , {name: "clicked", cond: (maybe "" identity state.clickedNodeId) == state.ast.id}
               ]
      in
      HH.span
        [ U.classNames_ cn
        , HE.onClick (handleApplyClick state.ast.id e1)
        ]
        [ HH.text "("
        , render $ state { ast = e1 }
        , HH.text " "
        , render $ state { ast = e2 }
        , HH.text ")"
        ]

handleApplyClick :: String -> Core.Node -> MouseEvent -> Maybe Action
handleApplyClick id e1 mouseEvent =
  case e1.expr of
    Core.Lambda _ _ ->
      Just $ ApplyClicked id mouseEvent
    _ ->
      Nothing

isApplicable :: Core.Node -> Boolean
isApplicable { id: _, expr: Core.Lambda _ _ } = true
isApplicable { id: _, expr: _ } = false

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction (InputUpdated input) = H.modify_ (const input)
handleAction (ApplyClicked id mouseEvent) = do
  _ <- liftEffect $ stopPropagation $ toEvent mouseEvent
  H.raise (Applied id)
