module Stepper (component) where

import Prelude

import Core as Core
import Parse as Parse
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH

type State = { lines :: Array Core.Node }

data Action = Initialize

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

render :: State -> H.ComponentHTML Action () Aff
render state = do
  HH.div 
    [] 
    [ HH.textarea []
    ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Initialize ->
    handleInitialize

initialize :: Maybe Action
initialize = Just Initialize

handleInitialize :: forall o. H.HalogenM State Action () o Aff Unit
handleInitialize = do
    let parsed = Parse.parse "((\\x.\\y.x a) b)"
    case parsed of
      Left err ->
        -- TODO handle parse error
        pure unit
      Right ast -> do
        ast' <- liftEffect $ Core.replaceIds ast
        H.modify_ (\s -> { lines: snoc s.lines ast' })
