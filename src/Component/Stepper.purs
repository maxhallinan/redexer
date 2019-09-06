module Component.Stepper (component) where

import Prelude

import Component.Node as Node
import Core as Core
import Parse as Parse
import Data.Array (mapWithIndex, snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = { lines :: Array Core.Node }

data Action = Initialize

type SlotIndex = Int

type ChildSlots = ( nodeSlot :: Node.Slot SlotIndex
                  )

_nodeSlot :: SProxy "nodeSlot"
_nodeSlot = SProxy

component :: forall q i o. H.Component HH.HTML q i o Aff
component = 
  H.mkComponent 
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction 
                                     , initialize = initialize
                                     }
    }

initialState :: State
initialState = { lines: mempty }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
  HH.div 
    [ HP.class_ $ HH.ClassName "stepper" ] 
    (mapWithIndex renderLine state.lines)

renderLine :: Int -> Core.Node -> H.ComponentHTML Action ChildSlots Aff
renderLine index ast = 
  HH.div 
    [ HP.class_ $ HH.ClassName "line" ] 
    [ HH.slot _nodeSlot index Node.component { ast } (const Nothing) ]

handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots o Aff Unit
handleAction = case _ of
  Initialize ->
    handleInitialize

initialize :: Maybe Action
initialize = Just Initialize

handleInitialize :: forall o. H.HalogenM State Action ChildSlots o Aff Unit
handleInitialize = do
    let parsed = Parse.parse "((\\x.\\y.x z) a)"
    case parsed of
      Left err -> do
        pure unit
      Right ast -> do
        ast' <- liftEffect $ Core.replaceIds ast
        H.modify_ (\s -> { lines: snoc s.lines ast' })
