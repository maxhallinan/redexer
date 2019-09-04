module Parse (ParseErr, parse) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Core (Node, Expr(..))
import Data.Either (Either)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.Language as L
import Text.Parsing.Parser.String as S
import Text.Parsing.Parser.Token as T

type Parser a = P.Parser String a

type ParseErr = P.ParseError

parse :: String -> Either P.ParseError Node
parse s = P.runParser s (C.between lexer.whiteSpace S.eof expr)

lexer :: T.TokenParser
lexer = T.makeTokenParser langDef

langDef :: T.LanguageDef
langDef = T.LanguageDef (T.unGenLanguageDef L.emptyDef)
  { identLetter = T.letter
  , identStart = T.letter
  }

expr :: Parser Node
expr = fix $ \p -> (C.try var) <|> (C.try $ lambda p) <|> (C.try $ apply' p)

node :: Parser Expr -> Parser Node
node e = do
  e' <- e
  pure $ { id: "", expr: e' }

var :: Parser Node
var = node $ Var <$> lexer.identifier

lambda :: Parser Node -> Parser Node
lambda e = node $ do
  _ <- lexer.lexeme $ S.char '\\'
  param <- lexer.identifier
  _ <- lexer.lexeme $ S.char '.'
  body  <- e
  pure $ Lambda param body

apply' :: Parser Node -> Parser Node
apply' e = node <<< lexer.parens $ do
  e1 <- e
  e2  <- e
  pure $ Apply e1 e2
