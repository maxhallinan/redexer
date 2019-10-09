module Term 
  ( Annotated(..)
  , Term
  , TermF(..)
  , TermType(..)
  , eval
  , isTermType
  , shift
  , showTerm
  , typeOf
  ) where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Free (Free)
import Data.Array as Array
import Data.Bifunctor (rmap)
import Data.Functor.Compose (Compose(..), bihoistCompose)
import Data.Functor.Mu (Mu(..))
import Data.Functor.Mu as Mu
import Matryoshka (cata, hylo, topDownCata)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

type Term = Annotated TermF

data Annotated f = Ann { id :: String } (f (Annotated f))

unAnn :: forall f. Annotated f -> f (Annotated f)
unAnn (Ann _ f) = f

instance eqTermF :: Eq t => Eq (TermF t) where
  eq (Var v1) (Var v2) = v1.index == v2.index
  eq (Fn a1) (Fn a2) = a1.body == a2.body
  eq (Apply l1 r1) (Apply l2 r2) = l1 == l2 && r1 == r2
  eq _ _ = false

type Context = Array String

showTerm :: Term -> String
showTerm = go []
  where 
  go ctx term = case unAnn term of
    Var { varName, index } ->
      indexToName ctx { name: varName, index }
    Fn { paramName, body } ->
      let
        fresh = pickFreshName ctx paramName
      in
      "(λ" <> fresh.name <> "." <> go fresh.ctx body <> ")"
    Apply left right ->
      "(" <> go ctx left <> " " <> go ctx right <> ")"

indexToName :: Context -> { name :: String, index :: Int } -> String
indexToName ctx { name, index } = 
  let
    isFree = index >= Array.length ctx
    isFresh = Array.findIndex (_ == name) ctx == Nothing
  in
  if isFree || isFresh
    then 
      if isFresh
        then name
        else name <> show index
      else 
        maybe name identity (Array.index ctx (Array.length ctx - index - 1))

pickFreshName :: Context -> String -> { ctx :: Context, name :: String }
pickFreshName ctx name = go 0 name
  where 
  go count n = 
    if Array.elemIndex n ctx == Nothing
      then { ctx: Array.snoc ctx n, name: n }
      else 
        let
          c = count + 1
        in
        go c (n <> show c)

data TermType = V | F | A
derive instance eqTermType :: Eq TermType

typeOf :: Term -> TermType
typeOf t = case unAnn t of
  Var _ -> V
  Fn _ -> F
  Apply _ _ -> A

isTermType :: TermType -> Term -> Boolean
isTermType termType term = typeOf term == termType

shift :: Int -> Term -> Term
shift inc term = go 0 term
  where
  go c t = case t of
    Ann ann (Var { varName, index }) ->
      if index >= c
        then Ann ann (Var { varName, index: index + inc })
        else Ann ann (Var { varName, index })
    Ann ann (Fn { paramName, body }) ->
      Ann ann (Fn { paramName, body: go (c + 1) body })
    Ann ann (Apply l r) ->
      Ann ann (Apply (go c l) (go c r))

substitute :: Int -> Term -> Term -> Term
substitute k sub term = go 0 term
  where 
  go c t = case t of
    Ann ann (Var { varName, index }) ->
      if index == k + c
        then shift c sub
        else Ann ann (Var { varName, index })
    Ann ann (Fn { paramName, body }) ->
      Ann ann (Fn { paramName, body: go (c + 1) body })
    Ann ann (Apply l r) ->
      Ann ann (Apply (go c l) (go c r))

subTop :: Term -> Term -> Term
subTop sub term = shift (-1) (substitute 0 (shift 1 sub) term)

eval :: Term -> Term
eval = case _ of
  Ann _ (Apply (Ann _ (Fn { body })) arg) ->
    subTop arg body
  Ann ann (Apply t1 t2) ->
    Ann ann (Apply (eval t1) (eval t2))
  Ann ann (Fn { paramName, body }) ->
    Ann ann (Fn { paramName, body: eval body })
  t ->
    t

type Term' = Mu TermF'

{-- type AnnTermF = Compose AnnF TermF --}

{-- data AnnF a = AnnF a Ann' --}

{-- derive instance functorAnnF :: Functor AnnF --}

type Ann' = { id :: String }

data TermF t
  = Var { varName :: String, index :: Int }
  | Fn { paramName :: String, body :: t }
  | Apply t t

derive instance functorTermF :: Functor TermF

data TermF' t
  = Var' { varName :: String, index :: Int } Ann'
  | Fn' { paramName :: String, body :: t } Ann'
  | Apply' t t Ann'

derive instance functorTermF' :: Functor TermF'

shift' :: Int -> Term' -> Term'
shift' inc term = cata (map Mu.roll <<< go) term 0
  where
  go t c = case t of
    Var' { varName, index } ann ->
      if index >= c
        then Var' {varName, index: index + inc} ann
        else Var' {varName, index} ann
    Fn' { paramName, body } ann ->
      Fn' { paramName, body: body (c + 1)} ann
    Apply' l r ann ->
      Apply' (l c) (r c) ann

substitute' :: Int -> Term' -> Term' -> Term'
substitute' k sub term = cata (map Mu.roll <<< go) term 0
  where
  go t c = case t of
    Var' { varName, index } ann ->
      if index == k + c
        then Mu.unroll (shift' c sub)
        else Var' { varName, index } ann
    Fn' { paramName, body } ann ->
      Fn' { paramName, body: body (c + 1) } ann
    Apply' l r ann ->
      Apply' (l c) (r c) ann

subTop' :: Term' -> Term' -> Term'
subTop' sub term = shift' (-1) (substitute' 0 (shift' 1 sub) term)
