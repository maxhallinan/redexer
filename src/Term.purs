module Term
  ( Ann
  , Term(..)
  , emptyAnn
  , eval
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (isNothing, maybe)

data Term
  = Var {varName :: String, index :: Int} Ann
  | Fn {paramName :: String, body :: Term} Ann
  | Apply Term Term Ann

instance showTerm :: Show Term where
  show = showTerm'

type Ann = {uuid :: String}

emptyAnn :: Ann
emptyAnn = {uuid: ""}

eval :: Term -> Term
eval = case _ of
  Apply (Fn {body} _) arg _ ->
    subTop arg body
  Apply t1 t2 ann ->
    Apply (eval t1) (eval t2) ann
  Fn {paramName, body} ann ->
    Fn {paramName, body: eval body} ann
  t ->
    t

subTop :: Term -> Term -> Term
subTop sub term = shift (-1) (substitute 0 (shift 1 sub) term)

{-
  _Types and Programming Languages_ by Benjamin C. Pierce, page 80
  [i -> s]var     = s     if var = i
  [i -> s]var     = var   otherwise
  [i -> s]λ.t     = λ.[i + 1 -> shift[1 0]s]t
  [i -> s](t1 t2) = ([i -> s]t1 [i -> s]t2)
-}
substitute :: Int -> Term -> Term -> Term
substitute k sub term = go 0 term
  where
  go c t = case t of
    Var {varName, index} ann ->
      if index == k + c
        then shift c sub
        else Var {varName, index} ann
    Fn {paramName, body} ann ->
      Fn {paramName, body: go (c + 1) body} ann
    Apply l r ann ->
      Apply (go c l) (go c r) ann

{-
  _Types and Programming Languages_ by Benjamin C. Pierce, page 79
  shift[i c]var     = var       if var < c
  shift[i c]var     = var + i   if var >= c
  shift[i c]λ.t     = λ.<shift[i c + 1]t>
  shift[i c](t1 t2) = (<shift[i c]t1> <shift[i c]t2>)
-}
shift :: Int -> Term -> Term
shift inc term = go 0 term
  where
  go c t = case t of
    Var {varName, index} ann ->
      if index >= c
        then Var {varName, index: index + inc} ann
        else Var {varName, index} ann
    Fn {paramName, body} ann ->
      Fn {paramName, body: go (c + 1) body} ann
    Apply l r ann ->
      Apply (go c l) (go c r) ann

type Context = Array String

showTerm' :: Term -> String
showTerm' = go []
  where
  go ctx term = case term of
    Var { varName, index } _ ->
      indexToName ctx {name: varName, index}
    Fn {paramName, body} _ ->
      let
        fresh = pickFreshName ctx paramName
      in
      "(λ" <> fresh.name <> "." <> go fresh.ctx body <> ")"
    Apply left right _ ->
      "(" <> go ctx left <> " " <> go ctx right <> ")"

indexToName :: Context -> {name :: String, index :: Int} -> String
indexToName ctx {name, index} =
  if not isFree
    then
      let
        {-
          λx.λy.λz.z y x = λλλ.0 1 2
          ctx = ["x", "y", "z"]
          x = 3 - 2 - 1 = 0
          y = 3 - 1 - 1 = 1
          z = 3 - 0 - 1 = 2
        -}
        offset = ctxSize - index - 1
      in
      maybe name identity (Array.index ctx offset)
    else if isFresh
      then name
      else name <> show index
  where
  ctxSize = Array.length ctx
  isFree = index >= ctxSize
  isFresh = isNothing $ Array.findIndex (_ == name) ctx

pickFreshName :: Context -> String -> {ctx :: Context, name :: String}
pickFreshName ctx name = go 0 name
  where
  go count n =
    if isNothing $ Array.elemIndex n ctx
      then {ctx: Array.snoc ctx n, name: n}
      else let c = count + 1 in
      go c (n <> show c)
