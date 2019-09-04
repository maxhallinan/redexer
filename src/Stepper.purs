module Stepper (component) where

import Prelude

import Core (Node)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component as HC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { lines :: Array Node }

data Action = Act

component :: forall q i o m. H.Component HH.HTML q i o m
component = 
  H.mkComponent 
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: State
initialState = { lines: mempty }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div 
    [] 
    [ HH.textarea []
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Act ->
    H.modify_ identity  
