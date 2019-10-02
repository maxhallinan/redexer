module Component.Util
  ( Offset
  , Range
  , className
  , classNames
  , classNames_
  , getSelectionRange
  , getSelectionRangeOffset
  , setFocus
  , setSelectionRange
  ) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried as F
import Effect (Effect)
import Effect.Uncurried as EU
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

setFocus :: String -> Effect Unit
setFocus = EU.runEffectFn1 _setFocus

getSelectionRange :: Int -> Effect Range
getSelectionRange = EU.runEffectFn1 _getSelectionRange

type Offset = { end :: Int, start :: Int }

getSelectionRangeOffset :: Range -> Offset
getSelectionRangeOffset = F.runFn1 _getSelectionRangeOffset

setSelectionRange :: String -> Offset -> Range -> Effect Unit
setSelectionRange = EU.runEffectFn3 _setSelectionRange

foreign import data Selection :: Type
foreign import data Range :: Type
foreign import _setFocus :: EU.EffectFn1 String Unit
foreign import _getSelectionRange :: EU.EffectFn1 Int Range
foreign import _getSelectionRangeOffset :: F.Fn1 Range Offset
foreign import _setSelectionRange :: EU.EffectFn3 String Offset Range Unit
