module Term 
  ( Annotated(..)
  , Term
  , TermF(..)
  , TermType(..)
  , isTermType
  , showTerm
  , typeOf
  ) where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)

type Term = Annotated { id :: String } TermF

data Annotated a f = Ann a (f (Annotated a f))

unAnn :: forall a f. Annotated a f -> f (Annotated a f)
unAnn (Ann _ f) = f

data TermF t
  = Var { varName :: String, index :: Int }
  | Fn { paramName :: String, body :: t }
  | Apply t t

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
    idIndex = (Array.length ctx) - index - 1
  in
  maybe name identity (Array.index ctx idIndex)

pickFreshName :: Context -> String -> { ctx :: Context, name :: String }
pickFreshName ctx name = go 0 name
  where go count n = 
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
