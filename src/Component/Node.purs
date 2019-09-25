module Component.Node (Slot, Input, Message(..), Query, component) where

import Prelude

import Core as Core
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Slot = H.Slot Query Message

type Input = { ast :: Core.Node, lineIndex :: Int }

data Action = ApplyClicked String MouseEvent

type State = { ast :: Core.Node, lineIndex :: Int }

data Message = Applied String

data Query a = Bloop

type ChildSlots = ()

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.mkComponent 
  { initialState: initState
  , render: renderNode
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                   }
  }

initState :: Input -> State
initState = identity

renderNode :: State -> H.ComponentHTML Action ChildSlots Aff
renderNode state =
  let
    edit = if state.lineIndex == 0
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
        [ HH.text $ "λ" <> param <> "."
        , render $ state { ast = body }
        ]
    Core.Apply e1 e2 ->
      let
          c = if isApplicable e1
              then "applicable"
              else ""
      in
      HH.span
        [ HP.classes [ HH.ClassName "apply", HH.ClassName c ]
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
handleAction (ApplyClicked id mouseEvent) = do
  _ <- liftEffect $ stopPropagation $ toEvent mouseEvent
  H.raise (Applied id)
