module Component.Node (Slot, Input, Message, Query, component) where

import Prelude

import Core as Core
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Slot = H.Slot Query Message

type Input = { ast :: Core.Node }

type Action = Unit

type State = { ast :: Core.Node }

type Message = Unit

data Query a = Bloop

type ChildSlots = ()

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.mkComponent 
  { initialState: initState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                   }
  }

initState :: Input -> State
initState = identity

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state = do
  case state.ast.expr of
    Core.Var varName ->
      HH.text varName
    Core.Lambda param body ->
      HH.span
        []
        [ HH.text $ "\\" <> param <> "."
        , render { ast: body }
        ]
    Core.Apply e1 e2 ->
      HH.span
        [ HP.class_ $ HH.ClassName "apply" ] 
        [ HH.text "(" 
        , render { ast: e1 }
        , HH.text " " 
        , render { ast: e2 }
        , HH.text ")" 
        ]

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction action = pure unit
