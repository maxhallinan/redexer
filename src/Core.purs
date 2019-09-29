module Core
  ( ApplyErr(..)
  , Expr(..)
  , ExprType(..)
  , Node
  , ReplaceErr(..)
  , applyLambda
  , betaReduction
  , eqNode
  , findNode
  , getApplyParts
  , isExprType
  , isReduceable
  , genIds
  , replaceNode
  ) where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), maybe)
import Data.UUID as U
import Effect (Effect)

type Node = { id :: String, expr :: Expr }

eqNode :: Node -> Node -> Boolean
eqNode n1 n2 = eq n1.expr n2.expr

data Expr
  = Var String
  | Lambda String Node
  | Apply Node Node

data ExprType = V | L | A

isExprType :: ExprType -> Expr -> Boolean
isExprType V (Var _ ) = true
isExprType L (Lambda _ _) = true
isExprType A (Apply _ _) = true
isExprType _ _ = false

instance showExpr :: Show Expr where
  show (Var s) = s
  show (Lambda p b) = "\\" <> p <> "." <> show b.expr
  show (Apply e1 e2) = "(" <> show e1.expr <> " " <> show e2.expr <> ")"

instance eqExpr :: Eq Expr where
  eq (Var s1) (Var s2) = s1 == s2
  eq (Lambda p1 b1) (Lambda p2 b2) = (p1 == p2) && (eqNode b1 b2)
  eq (Apply e1 e2) (Apply e3 e4) = (eqNode e1 e3) && (eqNode e2 e4)
  eq _ _ = false

data ReplaceErr = NodeNotFound String
derive instance eqReplaceErr :: Eq ReplaceErr

instance showReplaceErr :: Show ReplaceErr where
  show (NodeNotFound nodeId) = "(NodeNotFound " <> nodeId <> ")"

replaceNode :: String -> Node -> Node -> Either ReplaceErr Node
replaceNode id new old =
  let
      replace = replaceNode id new
  in
  case old.expr of
    Var _ ->
      if old.id == id
      then pure new
      else Left (NodeNotFound id)
    Lambda param body ->
      if old.id == id
      then pure new
      else do
        body' <- replace body
        pure { id: old.id, expr: Lambda param body' }
    Apply n1 n2 ->
      if old.id == id
      then pure new
      else do
        let toApplyNode x y = { id: old.id, expr: Apply x y }
        case replace n1 of
          Left _ -> do
            n2' <- replace n2
            pure $ toApplyNode n1 n2'
          Right n1' ->
            pure $ toApplyNode n1' n2

findNode :: String -> Node -> Maybe Node
findNode id tree =
  case tree.expr of
    Var _ ->
      if tree.id == id
      then pure tree
      else Nothing
    Lambda param body ->
      if tree.id == id
      then pure tree
      else findNode id body
    Apply e1 e2 ->
      if tree.id == id
      then pure tree
      else maybe (findNode id e2) pure $ findNode id e1

data ApplyErr = NotALambda
derive instance eqApplyErr :: Eq ApplyErr

instance showApplyErr :: Show ApplyErr where
  show NotALambda = "NotALambda"

applyLambda :: Node -> Node -> Either ApplyErr Node
applyLambda fn arg =
  case fn.expr of
    Lambda paramName body ->
      Right $ subParam paramName body
    _ ->
      Left NotALambda
  where
    subParam :: String -> Node -> Node
    subParam paramName body =
      case body.expr of
        Var varName ->
          if paramName == varName
          then arg
          else body
        Lambda p b ->
          { id: body.id
          , expr: Lambda p (subParam paramName b)
          }
        Apply n1 n2 ->
          { id: body.id
          , expr: Apply (subParam paramName n1) (subParam paramName n2)
          }

genIds :: Node -> Effect Node
genIds tree = do
  id <- U.toString <$> U.genUUID
  case tree.expr of
    Var _ ->
      pure $ { id: id, expr: tree.expr }
    Lambda param body -> do
      body' <- genIds body
      pure $ { id: id, expr: Lambda param body' }
    Apply e1 e2 -> do
       e1' <- genIds e1
       e2' <- genIds e2
       pure $ { id: id, expr: Apply e1' e2' }

getApplyParts :: Node -> Maybe { lambda :: Node, arg :: Node }
getApplyParts { id: _, expr: (Apply lambda arg) } = Just $ { lambda, arg }
getApplyParts _ = Nothing

betaReduction :: String -> Node -> Maybe Node
betaReduction nodeId tree = do
  node <- findNode nodeId tree
  { lambda, arg } <- getApplyParts node
  reduced <- hush $ applyLambda lambda arg
  newTree <- hush $ replaceNode nodeId reduced tree
  pure newTree

isReduceable :: Node -> Boolean
isReduceable { id: _, expr: Lambda _ _ } = true
isReduceable { id: _, expr: _ } = false
