module Component.Stepper (component) where

import Prelude
import Component.Step as Step
import Component.Util as U
import Control.Alt ((<|>))
import Data.Array (last, mapWithIndex, snoc)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Parse as Parse
import Term (Term)
import Term as Term

type Input
  = { defaultContent :: String
    }

type State
  = { currentTerm :: Maybe CurrentTerm
    , defaultContent :: String
    , steps :: Array Term
    , parseErr :: Maybe Parse.ParseErr
    , reductionOrder :: Array String
    , reductions :: Map String String
    }

type CurrentTerm
  = { stepIndex :: Int
    , termId :: String
    }

data Action
  = Initialize
  | Applied String
  | NewTerm { term :: Term }
  | CurrentTermChanged (Maybe CurrentTerm)
  | EditorOpened { stepIndex :: Int }
  | EditorClosed

type ChildSlots
  = ( stepSlot :: Step.Slot SlotIndex )

type SlotIndex
  = Int

_stepSlot :: SProxy "stepSlot"
_stepSlot = SProxy

component :: forall q o. H.Component HH.HTML q Input o Aff
component =
  H.mkComponent
    { eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    , initialState: initState
    , render
    }

initState :: Input -> State
initState { defaultContent } =
  { currentTerm: Nothing
  , defaultContent
  , steps: mempty
  , parseErr: Nothing
  , reductionOrder: mempty
  , reductions: mempty
  }

render :: State -> H.ComponentHTML Action ChildSlots Aff
render state =
  HH.div
    [ U.className "stepper" ]
    (mapWithIndex (renderStep state) state.steps)

renderStep :: State -> Int -> Term -> H.ComponentHTML Action ChildSlots Aff
renderStep state@{ currentTerm, steps, reductionOrder, reductions } i term =
  let
    stepPos = Step.toStepPos { current: i, total: length steps }

    stepInput =
      { term
      , focus: getStepFocus { stepIndex: i } state
      , stepIndex: i
      , stepPos
      , reducedTermId: Array.index reductionOrder i
      }

    cn =
      [ { name: "step", cond: true }
      , { name: "last", cond: stepPos == Step.Last || stepPos == Step.Only }
      ]
  in
    HH.div
      [ U.classNames_ cn ]
      [ HH.slot _stepSlot i Step.component stepInput handleMessage ]

getStepFocus :: { stepIndex :: Int } -> State -> Maybe Step.Focus
getStepFocus { stepIndex } state =
  getSuccessFocus stepIndex state
    <|> getTodoFocus stepIndex state
    <|> getDoneFocus stepIndex state

getSuccessFocus :: Int -> State -> Maybe Step.Focus
getSuccessFocus stepIndex state = do
  currentTerm <- state.currentTerm
  if currentTerm.stepIndex == stepIndex then do
    step <- Array.index state.steps stepIndex
    reducedId <- Array.index state.reductionOrder stepIndex
    reducedTerm <- Term.findTerm reducedId step
    if Term.isDescendantOf currentTerm.termId reducedTerm then
      pure $ makeFocus reducedId
    else
      Nothing
  else
    if currentTerm.stepIndex + 1 == stepIndex then do
      step <- Array.index state.steps currentTerm.stepIndex
      reducedId <- Array.index state.reductionOrder currentTerm.stepIndex
      reducedTerm <- Term.findTerm reducedId step
      successId <- Map.lookup reducedId state.reductions
      if Term.isDescendantOf currentTerm.termId reducedTerm then
        pure $ makeFocus successId
      else
        Nothing
    else
      Nothing
  where
  makeFocus termId = { termId, highlight: Step.Success }

getTodoFocus :: Int -> State -> Maybe Step.Focus
getTodoFocus stepIndex state = do
  currentTerm <- state.currentTerm
  step <- Array.index state.steps stepIndex
  term <- Term.findTerm currentTerm.termId step
  nearestRedex <- Term.closestRedexAncestor (Term.uuid term) step
  let
    isReduced = isJust $ Array.index state.reductionOrder stepIndex
  if stepIndex == currentTerm.stepIndex && not isReduced then
    pure $ makeFocus (Term.uuid nearestRedex)
  else
    Nothing
  where
  makeFocus termId = { termId, highlight: Step.Todo }

getDoneFocus :: Int -> State -> Maybe Step.Focus
getDoneFocus stepIndex state = do
  termId <- Array.index state.reductionOrder stepIndex
  pure { termId, highlight: Step.Done }

handleMessage :: Step.Message -> Maybe Action
handleMessage = case _ of
  Step.Applied id -> Just (Applied id)
  Step.EditorOpened { stepIndex } -> Just (EditorOpened { stepIndex })
  Step.EditorClosed -> Just EditorClosed
  Step.NewTerm { term } -> Just (NewTerm { term })
  Step.TermHoverOn currentTerm -> Just $ CurrentTermChanged (Just currentTerm)
  Step.TermHoverOff -> Just $ CurrentTermChanged Nothing

handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots o Aff Unit
handleAction action = case action of
  Initialize -> handleInitialize
  Applied id -> handleApplied id
  EditorOpened { stepIndex } -> handleEditorOpened stepIndex
  EditorClosed -> handleEditorClosed
  NewTerm { term } -> handleNewTerm term
  CurrentTermChanged currentTerm -> handleCurrentTerm currentTerm

handleEditorOpened :: forall o. Int -> H.HalogenM State Action ChildSlots o Aff Unit
handleEditorOpened stepIndex = void $ H.queryAll _stepSlot (H.tell $ Step.Disable { stepIndex })

handleEditorClosed :: forall o. H.HalogenM State Action ChildSlots o Aff Unit
handleEditorClosed = void $ H.queryAll _stepSlot (H.tell Step.Enable)

handleCurrentTerm :: forall o. Maybe CurrentTerm -> H.HalogenM State Action ChildSlots o Aff Unit
handleCurrentTerm currentTerm = H.modify_ \s -> s { currentTerm = currentTerm }

handleApplyHoverOff :: forall o. H.HalogenM State Action ChildSlots o Aff Unit
handleApplyHoverOff = H.modify_ \s -> s { currentTerm = Nothing }

handleApplied :: forall o. String -> H.HalogenM State Action ChildSlots o Aff Unit
handleApplied termId = do
  newStep <- reduceLastStep
  maybe return updateState newStep
  where
  return = pure unit

  reduceLastStep = H.gets $ _.steps >>> last >=> Term.reduce termId

  updateState { step, term } = do
    H.modify_ \s ->
      s
        { steps = snoc s.steps term
        , currentTerm = Just $ { stepIndex: (length s.steps) - 1, termId }
        , reductionOrder = snoc s.reductionOrder termId
        , reductions = Map.insert termId (Term.uuid step) s.reductions
        }

handleInitialize :: forall o. H.HalogenM State Action ChildSlots o Aff Unit
handleInitialize = do
  defaultContent <- H.gets _.defaultContent
  case Parse.parse defaultContent of
    Left err -> pure unit
    Right firstStep -> do
      l <- genIds firstStep
      H.modify_ \s -> s { steps = pure l }
  where
  genIds = liftEffect <<< Term.genIds

handleNewTerm :: forall o. Term -> H.HalogenM State Action ChildSlots o Aff Unit
handleNewTerm term = do
  l <- genIds term
  H.modify_ \state ->
    state
      { currentTerm = Nothing
      , steps = pure l
      , parseErr = Nothing
      , reductionOrder = mempty :: Array String
      , reductions = mempty :: Map String String
      }
  where
  genIds = liftEffect <<< Term.genIds
