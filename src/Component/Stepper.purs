module Component.Stepper (component) where

import Prelude

import Component.Node as Node
import Control.Apply (lift2)
import Core as Core
import Parse as Parse
import Data.Array ((:), head, mapWithIndex, reverse, snoc)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = { lines :: Array Core.Node }

data Action = Initialize | Applied String

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
    (mapWithIndex renderLine $ reverse state.lines)

renderLine :: Int -> Core.Node -> H.ComponentHTML Action ChildSlots Aff
renderLine index ast =
  HH.div
    [ HP.class_ $ HH.ClassName "line" ]
    [ HH.slot _nodeSlot index Node.component { ast, lineIndex: index } handleMessage ]

handleMessage :: Node.Message -> Maybe Action
handleMessage (Node.Applied id) = Just $ Applied id

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
                  H.modify_ (\s -> { lines: (newTree : s.lines)})
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
    let parsed = Parse.parse "((\\x.\\y.\\s.\\z.((x s) ((y s) z)) \\s.\\z.(s z)) \\s.\\z.(s (s z)))"
    case parsed of
      Left err -> do
        pure unit
      Right ast -> do
        ast' <- liftEffect $ Core.replaceIds ast
        H.modify_ (\s -> { lines: ast' : s.lines })