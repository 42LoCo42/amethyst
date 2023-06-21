{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Prelude hiding (many, some)

import Flow                 ((.>), (|>))
import Text.Megaparsec      (MonadParsec (label), Parsec, anySingle, choice,
                             many, noneOf, oneOf, some, (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, hexDigitChar, string)

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

-- | Read some hex digits and convert them to their corresponding character.
-- TODO: chr throws an exception if the value is not in range
-- (from testing: negative or greather than 0x10FFFF)
-- FIX: check this range before passing to chr and convert to Nothing on error
hexDigitsToChar :: [Char] -> Parser Char
hexDigitsToChar =
  ("0x" ++)                           -- read needs a full hexadecimal number
  .> (\x -> readEither x <&> chr)     -- read & convert number to char
  .> either (toString .> fail) return -- handle read error

-- | Parse a symbol, which consists of alphanumeric or allowed special characters.
pSymbol :: Parser AST
pSymbol = (
  choice [ alphaNumChar, oneOf allowedSpecials ]
  |> label "allowed symbol char"
  |> some
  ) <&> toText .> ASTSymbol

-- | Parse an escaped char. All escape sequences from ascii(7) are recognized,
-- also \e results in the ESC character (0x1B) and \x??, \u???? and \U??????
-- allow direct codepoint input (where ? is a hex digit) (TODO)
pEscapedChar :: Parser Char
pEscapedChar =
  char '\\' >> choice
  [ char 'x' >> replicateM 2 hexDigitChar >>= hexDigitsToChar
  , char 'u' >> replicateM 4 hexDigitChar >>= hexDigitsToChar
  , char 'U' >> replicateM 6 hexDigitChar >>= hexDigitsToChar

  , char '0'  $> '\0'
  , char 'a'  $> '\a'
  , char 'b'  $> '\b'
  , char 't'  $> '\t'
  , char 'n'  $> '\n'
  , char 'v'  $> '\v'
  , char 'f'  $> '\f'
  , char 'r'  $> '\r'
  , char 'e'  $> '\ESC' -- custom, not in ascii(7)

  , string "NUL" $> '\NUL'
  , string "SOH" $> '\SOH'
  , string "STX" $> '\STX'
  , string "ETX" $> '\ETX'
  , string "EOT" $> '\EOT'
  , string "ENQ" $> '\ENQ'
  , string "ACK" $> '\ACK'
  , string "BEL" $> '\BEL'
  , string "BS"  $> '\BS'
  , string "HT"  $> '\HT'
  , string "LF"  $> '\LF'
  , string "VT"  $> '\VT'
  , string "FF"  $> '\FF'
  , string "CR"  $> '\CR'
  , string "SO"  $> '\SO'
  , string "SI"  $> '\SI'
  , string "DLE" $> '\DLE'
  , string "DC1" $> '\DC1'
  , string "DC2" $> '\DC2'
  , string "DC3" $> '\DC3'
  , string "DC4" $> '\DC4'
  , string "NAK" $> '\NAK'
  , string "SYN" $> '\SYN'
  , string "ETB" $> '\ETB'
  , string "CAN" $> '\CAN'
  , string "EM"  $> '\EM'
  , string "SUB" $> '\SUB'
  , string "ESC" $> '\ESC'
  , string "FS"  $> '\FS'
  , string "GS"  $> '\GS'
  , string "RS"  $> '\RS'
  , string "US"  $> '\US'

  , anySingle {- any character can be escaped like this -} ]

-- | Parse a character literal.
pChar :: Parser Char
pChar =
  choice [ pEscapedChar, noneOf ['\''] <?> "any character but '" ]
  |> (char '\'' *>)
  |> (<* char '\'')

-- | Parse a text/string literal.
pText :: Parser Text
pText = (
  choice [ pEscapedChar, noneOf ['"'] <?> "any character but \"" ]
  |> many
  |> (char '"' *>)
  |> (<* char '"')
  ) <&> toText
