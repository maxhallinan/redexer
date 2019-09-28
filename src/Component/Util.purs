module Component.Util 
  ( className
  , classNames
  , classNames_
  ) where

import Prelude

import Data.Array as Array
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties as HP

className :: forall r i. String -> IProp (class :: String | r) i
className = HP.class_ <<< ClassName

classNames :: forall r i. Array String -> IProp (class :: String | r) i
classNames = HP.classes <<< map ClassName

classNames_ :: forall r i. Array { cond :: Boolean, name :: String } -> IProp (class :: String | r) i
classNames_ = 
  Array.filter _.cond
  >>> map (ClassName <<< _.name)
  >>> HP.classes
