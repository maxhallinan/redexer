module Parse
  ( ParseErr
  , parse
  , errMsg
  , errPos
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.Array as Array
import Data.Char.Unicode (isAlpha)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Term (Term(..), emptyAnn)
import Text.Parsing.Parser (ParseError, ParserT)
import Text.Parsing.Parser as Parser
import Text.Parsing.Parser.Combinators as Comb
import Text.Parsing.Parser.Pos as Pos
import Text.Parsing.Parser.String as Str
import Text.Parsing.Parser.Token as Tok

type Parser a
  = ParserT String ParseState a

type ParseState
  = State Context

type Context
  = { bound :: Array String
    , free :: Array String
    }

initialContext :: Context
initialContext =
  { bound: []
  , free: []
  }

type ParseErr
  = ParseError

parse :: String -> Either ParseErr Term
parse =
  flip Parser.runParserT parser
    >>> flip State.evalState initialContext
  where
  parser = Comb.between lexer.whiteSpace Str.eof termParser

termParser :: Parser Term
termParser =
  implicitParens
    $ fix \t ->
        varParser
          <|> tryParens (fnParser t)
          <|> fnParser t
          <|> tryParens (applyParser t)
  where
  tryParens = Comb.try <<< lexer.parens

implicitParens :: Parser Term -> Parser Term
implicitParens t = do
  first <- t
  rest <- Array.many t
  case rest of
    [] -> pure first
    _ -> pure $ Array.foldl toApply first rest
  where
  toApply left right = Apply left right emptyAnn

varParser :: Parser Term
varParser = do
  varName <- lexer.identifier
  index <- assignIndex varName
  pure $ Var { varName, index } emptyAnn

fnParser :: Parser Term -> Parser Term
fnParser t = do
  _ <- lexer.lexeme $ Str.oneOf [ 'λ', '\\' ]
  paramName <- lexer.identifier
  _ <- lexer.lexeme $ Str.char '.'
  body <-
    withLocalCtx do
      bindParam paramName
      implicitParens t
  pure $ Fn { paramName, body } emptyAnn
  where
  withLocalCtx p = do
    ctx <- State.lift State.get
    x <- p
    State.lift $ State.put ctx
    pure x

  bindParam name = State.lift $ State.modify_ \ctx -> ctx { bound = Array.cons name ctx.bound }

applyParser :: Parser Term -> Parser Term
applyParser t = do
  t1 <- t
  t2 <- t
  let
    innerMost = Apply t1 t2 emptyAnn
  tn <- Array.many t
  case tn of
    [] -> pure innerMost
    _ -> pure $ Array.foldl toApply innerMost tn
  where
  toApply t1 t2 = Apply t1 t2 emptyAnn

assignIndex :: String -> Parser Int
assignIndex varName = do
  { bound, free } <- State.lift State.get
  let
    boundIndex = findIndex bound
  let
    freeIndex = findIndex free
  case boundIndex <|> freeIndex of
    Nothing -> nextFreeIndex
    Just index -> pure index
  where
  findIndex = Array.findIndex (_ == varName)

  nextFreeIndex = do
    { bound, free } <- State.lift $ State.modify \ctx -> ctx { free = Array.cons varName ctx.free }
    pure $ (Array.length free + Array.length bound) - 1

lexer :: Tok.GenTokenParser String ParseState
lexer = Tok.makeTokenParser langDef

langDef :: Tok.GenLanguageDef String ParseState
langDef =
  Tok.LanguageDef
    { commentStart: ""
    , commentEnd: ""
    , commentLine: ""
    , nestedComments: true
    , identLetter: Str.oneOf $ toCharArray "0123456789"
    , identStart: alphaNotLambda
    , opStart: Str.oneOf []
    , opLetter: Str.oneOf []
    , reservedOpNames: []
    , reservedNames: []
    , caseSensitive: true
    }

alphaNotLambda :: Parser Char
alphaNotLambda = Str.satisfy $ \c -> isAlpha c && c /= 'λ'

errMsg :: ParseErr -> String
errMsg = Parser.parseErrorMessage

errPos :: ParseErr -> { column :: Int, line :: Int }
errPos = Parser.parseErrorPosition >>> \(Pos.Position pos) -> pos
