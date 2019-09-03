module Core(ApplyErr(..), Expr(..), Node, applyLambda, eqNode) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple)

type Node = { id :: String, expr :: Expr }

eqNode :: Node -> Node -> Boolean
eqNode n1 n2 = eq n1.expr n2.expr


data Expr 
  = Var String
  | Lambda String Node
  | Apply Node Node

instance showExpr :: Show Expr where
  show (Var s) = s
  show (Lambda p b) = "\ " <> p <> ". " <> show b
  show (Apply e1 e2) = "(" <> show e1 <> " " <> show e2 <> ")"

instance eqExpr :: Eq Expr where
  eq (Var s1) (Var s2) = s1 == s2
  eq (Lambda p1 b1) (Lambda p2 b2) = (p1 == p2) && (eqNode b1 b2)
  eq (Apply e1 e2) (Apply e3 e4) = (eqNode e1 e3) && (eqNode e2 e4)
  eq _ _ = false

data ApplyErr = NotALambda

instance showApplyErr :: Show ApplyErr where
  show NotALambda = "Not a lambda."

instance eqApplyErr :: Eq ApplyErr where
  eq NotALambda NotALambda = true

applyLambda :: Node -> Node -> Either ApplyErr Node
applyLambda fn arg =
  case fn.expr of
    Lambda paramName body ->
      Right $ subParam paramName body arg
    _ ->
      Left NotALambda
  where 
    subParam :: String -> Node -> Node -> Node
    subParam paramName body arg =
      case body.expr of
        Var varName ->
          if paramName == varName
          then arg
          else body
        Lambda p b ->
          { id: body.id, expr: Lambda p (subParam paramName b arg) }
        Apply n1 n2 ->
          { id: body.id, expr: Apply (subParam paramName n1 arg) (subParam paramName n2 arg) }
