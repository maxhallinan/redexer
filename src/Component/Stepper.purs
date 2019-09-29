module Component.Stepper (component) where

import Prelude

import Component.Node as Node
import Component.Util as U
import Core as Core
import Data.Array (last, mapWithIndex, snoc)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Foldable (length)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Parse as Parse

type Input =
  { defaultContent :: String
  }

type State =
  { defaultContent :: String
  , lines :: Array Core.Node
  , reducedNodes :: Array String
  }

data Action = Initialize | Applied String

type ChildSlots = ( nodeSlot :: Node.Slot SlotIndex )

type SlotIndex = Int

_nodeSlot :: SProxy "nodeSlot"
_nodeSlot = SProxy

component :: forall q o. H.Component HH.HTML q Input o Aff
component =
  H.mkComponent
    { eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , initialize = Just Initialize
                                     }
    , initialState: initState
    , render
    }

initState :: Input -> State
initState { defaultContent } =
  { defaultContent
  , lines: mempty
  , reducedNodes: mempty
  }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
  HH.div
    [ U.className "stepper" ]
    (mapWithIndex (renderLine state) state.lines)

renderLine :: State -> Int -> Core.Node -> H.ComponentHTML Action ChildSlots Aff
renderLine { lines, reducedNodes } i ast =
  let
      linePos = Node.toLinePos { current: i, total: length lines }

      nodeInput =
        { ast
        , reducedNodeId: Array.index reducedNodes i
        , linePos
        }

      cn = [ { name: "line", cond: true }
           , { name: "last", cond: linePos == Node.Last || linePos == Node.Only }
           ]
  in
  HH.div
    [ U.classNames_ cn ]
    [ HH.slot _nodeSlot i Node.component nodeInput handleMessage ]

handleMessage :: Node.Message -> Maybe Action
handleMessage (Node.Applied id) = Just (Applied id)

handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots o Aff Unit
handleAction action = case action of
  Initialize -> do
    handleInitialize
  Applied id -> do
    handleApplied id

handleApplied :: forall o. String -> H.HalogenM State Action ChildSlots o Aff Unit
handleApplied nodeId = do
  newLine <- reduceLastLine
  maybe return updateState newLine
  where
    return = pure unit
    reduceLastLine = H.gets $ _.lines >>> last >=> Core.betaReduction nodeId
    updateState newLine = do
       H.modify_ \s -> s { lines = snoc s.lines newLine
                         , reducedNodes = snoc s.reducedNodes nodeId
                         }

handleInitialize :: forall o. H.HalogenM State Action ChildSlots o Aff Unit
handleInitialize = do
  defaultContent <- H.gets _.defaultContent
  case Parse.parse defaultContent of
    Left err -> do
      pure unit
    Right firstLine -> do
      l <- genIds firstLine
      H.modify_ \s -> s { lines = pure l }
  where genIds = liftEffect <<< Core.genIds
