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
  , setLineBreak
  ) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried as F
import Effect (Effect)
import Effect.Uncurried as EU
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Properties (IProp)
import Halogen.HTML.Properties as HP
import Web.DOM.Node as DOM

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

setSelectionRange :: String -> { before :: String, highlight :: String, after :: String } -> Effect Unit
setSelectionRange = EU.runEffectFn2 _setSelectionRange

setLineBreak :: DOM.Node -> Effect Unit
setLineBreak = EU.runEffectFn1 _setLineBreak

foreign import data Selection :: Type
foreign import data Range :: Type
foreign import _setFocus :: EU.EffectFn1 String Unit
foreign import _getSelectionRange :: EU.EffectFn1 Int Range
foreign import _getSelectionRangeOffset :: F.Fn1 Range Offset
foreign import _setSelectionRange :: EU.EffectFn2 String { before :: String, highlight :: String, after :: String } Unit
foreign import _setLineBreak :: EU.EffectFn1 DOM.Node Unit

deleteEditorContent :: DOM.Node -> Effect Unit
deleteEditorContent = EU.runEffectFn1 _deleteEditorContent

highlightErPos :: { before :: String, highlight :: String, after :: String } -> DOM.Node -> Effect Unit
highlightErPos = EU.runEffectFn2 _highlightErrPos

setEditorTextContent :: String -> DOM.Node -> Effect Unit
setEditorTextContent = EU.runEffectFn2 _setEditorTextContent

foreign import _deleteEditorContent :: EU.EffectFn1 DOM.Node Unit
foreign import _highlightErrPos :: EU.EffectFn2 { before :: String, highlight :: String, after :: String } DOM.Node Unit
foreign import _setEditorTextContent :: EU.EffectFn2 String DOM.Node Unit
