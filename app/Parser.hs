module Parser (pSymbol) where

import Prelude hiding (some)

import Flow                 ((.>), (|>))
import Text.Megaparsec      (MonadParsec (label), Parsec, choice, oneOf, some)
import Text.Megaparsec.Char (alphaNumChar)

import Types (AST (..))

type Parser = Parsec Void Text

-- | The list of all special characters allowed in a symbol.
allowedSpecials :: [Char]
allowedSpecials =
  [ '!' -- single ! is special, but still allowed in symbol (e.g. for mutating functions)
  -- , '"' -- for strings
  , '#'
  , '$'
  , '%'
  , '&'
  , '\'' -- TODO maybe make ' special for thunk/simple quote
  -- , '(' -- for tuples
  -- , ')' -- for tuples
  , '*'
  , '+'
  -- , ',' -- TODO maybe make , special for unquote? depends on macro system
  , '-'
  -- , '.' -- for element access
  , '/'
  , ':'
  , ';'
  , '<'
  , '='
  , '>'
  , '?'
  , '@'
  -- , '[' -- for arrays
  , '\\'
  -- , ']' -- for arrays
  , '^'
  , '_'
  , '`' -- TODO maybe make ` special for metaquote? depends on macro system
  -- , '{' -- for blocks
  , '|' -- TODO maybe make pipe a special operator?
  -- , '}' -- for blocks
  , '~' ]

-- | Parse a symbol, which consists of alphanumeric or allowed special characters.
pSymbol :: Parser AST
pSymbol = (
    choice [alphaNumChar, oneOf allowedSpecials]
    |> label "allowed symbol char"
    |> some
  ) <&> toText .> ASTSymbol
