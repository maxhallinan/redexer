module Component.Stepper (component) where

import Prelude

import Component.Node as Node
import Component.Util as U
import Core as Core
import Data.Array ((:), head, index, mapWithIndex, reverse, snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
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
  { clickedNodes :: Array String
  , defaultContent :: String
  , lines :: Array Core.Node
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
                                     , initialize = initialize
                                     }
    , initialState: initState
    , render
    }

initState :: Input -> State
initState { defaultContent } =
  { clickedNodes: mempty
  , defaultContent
  , lines: mempty
  }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
  HH.div
    [ U.className "stepper" ]
    (mapWithIndex (renderLine state) $ reverse state.lines)

renderLine :: State -> Int -> Core.Node -> H.ComponentHTML Action ChildSlots Aff
renderLine state i ast =
  let
      isLast = i == length state.lines - 1

      nodeInput =
        { ast
        , clickedNodeId: index state.clickedNodes i
        , isLast
        , lineIndex: i
        }

      cn = [ { name: "line", cond: true }
           , { name: "last", cond: isLast }
           ]
  in
  HH.div
    [ U.classNames_ cn ]
    [ HH.slot _nodeSlot i Node.component nodeInput handleNodeMessage ]

handleNodeMessage :: Node.Message -> Maybe Action
handleNodeMessage (Node.Applied id) = Just $ Applied id

handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots o Aff Unit
handleAction action = case action of
  Initialize -> do
    handleInitialize
  Applied id -> do
    handleApplied id

handleApplied :: forall o. String -> H.HalogenM State Action ChildSlots o Aff Unit
handleApplied id = do
  mLine <- H.gets lastLine
  case mLine of
    Just line -> do
      let mNode = Core.findNode id line
      let mE1 = mNode >>= toE1
      let mE2 = mNode >>= toE2
      case Tuple mE1 mE2 of
        Tuple (Just e1) (Just e2) ->
          case Core.applyLambda e1 e2 of
            Left err -> do
              pure unit
            Right tree ->
              case Core.replaceNode id tree line of
                Left err -> do
                  pure unit
                Right newTree ->
                  H.modify_ (\s -> s { clickedNodes = snoc s.clickedNodes id, lines = (newTree : s.lines)})
        _ -> do
          pure unit
    Nothing -> do
      pure unit
  where
    lastLine = head <<< _.lines
    findNode = Core.findNode id

    toE1 :: Core.Node -> Maybe Core.Node
    toE1 { id: _, expr: (Core.Apply e1 _) } = Just $ e1
    toE1 _ = Nothing

    toE2 :: Core.Node -> Maybe Core.Node
    toE2 { id: _, expr: (Core.Apply _ e2) } = Just $ e2
    toE2 _ = Nothing

initialize :: Maybe Action
initialize = Just Initialize

handleInitialize :: forall o. H.HalogenM State Action ChildSlots o Aff Unit
handleInitialize = do
    defaultContent <- H.gets _.defaultContent
    let parsed = Parse.parse defaultContent
    case parsed of
      Left err -> do
        pure unit
      Right ast -> do
        ast' <- liftEffect $ Core.replaceIds ast
        H.modify_ (\s -> s{ lines = ast' : s.lines })
