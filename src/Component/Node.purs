module Component.Node (Input, Message, Query, Slot, component) where

import Prelude

import Core as Core
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

type Slot = H.Slot Query Message

data Query a = NoOp

data Message = NodeClicked

type Input = { ast :: Core.Node }

data Action = Initialize

type State = Int

type ChildSlots = ()

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.mkComponent 
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                   }
  }

initialState :: State
initialState = 1

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state = do
  HH.div
    []
    [ HH.text "node component" ]

handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction action = pure unit
