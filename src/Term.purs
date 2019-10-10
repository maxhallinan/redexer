module Term
  ( Ann
  , Context
  , Term(..)
  , closestRedexAncestor
  , emptyAnn
  , findTerm
  , genIds
  , indexToName
  , isDescendantOf
  , isRedex
  , pickFreshName
  , reduce
  , step
  , uuid
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.UUID as U
import Effect (Effect)

data Term
  = Var {varName :: String, index :: Int} Ann
  | Fn {paramName :: String, body :: Term} Ann
  | Apply Term Term Ann

instance showTerm :: Show Term where
  show = showTermImpl

type Ann = {uuid :: String}

emptyAnn :: Ann
emptyAnn = {uuid: ""}

step :: Term -> Term
step = case _ of
  Apply (Fn {body} _) arg _ ->
    subOuter arg body
  Apply t1 t2 ann ->
    Apply (step t1) (step t2) ann
  Fn {paramName, body} ann ->
    Fn {paramName, body: step body} ann
  t ->
    t

subOuter :: Term -> Term -> Term
subOuter sub term = shift (-1) (substitute 0 (shift 1 sub) term)

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

showTermImpl :: Term -> String
showTermImpl = go []
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
      go c (name <> show c)

genIds :: Term -> Effect Term
genIds = case _ of
  Var v ann -> do
    a <- withUuid ann
    pure $ Var v a
  Fn {paramName, body} ann -> do
    a <- withUuid ann
    b <- genIds body
    pure $ Fn {paramName, body: b} a
  Apply left right ann -> do
    a <- withUuid ann
    l <- genIds left
    r <- genIds right
    pure $ Apply l r a
  where
  withUuid ann = do
    uuid <- U.toString <$> U.genUUID
    pure ann{uuid = uuid}

reduce :: String -> Term -> Maybe {step :: Term, term :: Term}
reduce uuid term =
  findTerm uuid term
  # map step
  # map (\nextStep -> {step: nextStep, term: replaceTerm uuid nextStep term})

reduceBy :: (Term -> Term) -> String -> Term -> Term
reduceBy reducer uuid term = case term of
  Var _ ann ->
    if ann.uuid == uuid
      then reducer term
      else term
  Fn fn ann ->
    if ann.uuid == uuid
      then reducer term
      else Fn fn{body = reduceBy reducer uuid fn.body} ann
  Apply left right ann ->
    if ann.uuid == uuid
      then reducer term
      else Apply (reduceBy reducer uuid left) (reduceBy reducer uuid right) ann

termId :: Term -> String
termId = _.uuid <<< toAnn

toAnn :: Term -> Ann
toAnn = case _ of
  Var _ ann ->
    ann
  Fn _ ann ->
    ann
  Apply _ _ ann ->
    ann

isRedex :: Term -> Boolean
isRedex = case _ of
  Apply (Fn _ _) _ _ ->
    true
  _ ->
    false

findTerm :: String -> Term -> Maybe Term
findTerm uuid term = case term of
  Var _ ann ->
    if ann.uuid == uuid
      then Just term
      else Nothing
  Fn {body} ann ->
    if ann.uuid == uuid
      then Just term
      else findTerm uuid body
  Apply l r ann ->
    if ann.uuid == uuid
      then Just term
      else (findTerm uuid l) <|> (findTerm uuid r)

replaceTerm :: String -> Term -> Term -> Term
replaceTerm uuid new term = case term of
  Var var ann ->
    if uuid == ann.uuid
      then new
      else Var var ann
  Fn {paramName, body} ann ->
    if uuid == ann.uuid
      then new
      else Fn {paramName, body: replaceTerm uuid new body} ann
  Apply l r ann ->
    if uuid == ann.uuid
      then new
      else Apply (replaceTerm uuid new l) (replaceTerm uuid new r) ann

isDescendantOf :: String -> Term -> Boolean
isDescendantOf uuid term = case term of
  Var _ ann ->
    uuid == ann.uuid
  Fn {body} ann ->
    uuid == ann.uuid || isDescendantOf uuid body
  Apply l r ann ->
    uuid == ann.uuid || (isDescendantOf uuid l) || (isDescendantOf uuid r)

closestRedexAncestor :: String -> Term -> Maybe Term
closestRedexAncestor uuid term = 
  if isRedex term
    then Just term
    else go Nothing term
  where
  go closest subterm = case subterm of
    Var _ ann ->
      if uuid == ann.uuid
        then closest
        else Nothing
    Fn {body} ann ->
      if uuid == ann.uuid
        then closest
        else go closest body
    Apply l r ann ->
      if uuid == ann.uuid
        then closest
        else let 
          nextClosest = 
            if isRedex subterm 
              then Just subterm 
              else closest 
        in
        (go nextClosest l) <|> (go nextClosest r)

uuid :: Term -> String
uuid = case _ of
  Var _ ann ->
    ann.uuid
  Fn _ ann ->
    ann.uuid
  Apply _ _ ann ->
    ann.uuid
