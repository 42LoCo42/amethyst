{-# LANGUAGE OverloadedStrings #-}
module Parser (pAST, pAST') where

import Prelude hiding (many, some)

import Flow ((.>), (|>))

import Data.Char  (digitToInt, isDigit, isHexDigit, isOctDigit)
import Data.Ratio ((%))

import Text.Megaparsec            (MonadParsec (hidden, label, try), Parsec,
                                   anySingle, choice, many, noneOf, oneOf,
                                   satisfy, sepEndBy, skipMany, some, (<?>))
import Text.Megaparsec.Char       (alphaNumChar, char, hexDigitChar, hspace1,
                                   space1, string)
import Text.Megaparsec.Char.Lexer (skipBlockCommentNested, skipLineComment)
import Text.Megaparsec.Debug      (dbg)

import Types (AST (..))

type Parser = Parsec Void Text

dbg' :: Show a => String -> Parser a -> Parser a
-- dbg' = flip const
dbg' = dbg

-- | Parse a single AST node.
pAST :: Parser AST
pAST = pAST' id

-- | Parse a single AST node under a transformation of the individual parsers.
pAST' :: (Parser AST -> Parser AST) -> Parser AST
pAST' transform =
  [ dbg' "char" pChar   <&> ASTChar
  , dbg' "text" pText   <&> ASTText
  , dbg' "frac" pFrac   <&> ASTFrac
  , dbg' "int " pInt    <&> ASTInt
  , dbg' "sym " pSymbol <&> ASTSymbol
  , dbg' "tup " pTup    <&> ASTTup
  , dbg' "arr " pArr    <&> ASTArr
  , dbg' "blk " pBlk    <&> ASTBlk ]
  <&> (transform .> try)
  |> choice

-- Utilities -------------------------------------------------------------------

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
  , '.' -- for element access
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
pSymbol :: Parser Text
pSymbol = (
  choice [ alphaNumChar, oneOf allowedSpecials ]
  |> label "allowed symbol char"
  |> some
  ) <&> toText

-- Parsers ---------------------------------------------------------------------

-- | Parse a line comment, starting with a #. The space is mandatory.
pLineComment :: Parser ()
pLineComment = skipLineComment "# "

-- | Parse a block comment (literally a {} block with comment/# signs).
pBlockComment :: Parser ()
pBlockComment = skipBlockCommentNested "#{" "}#"

-- | Parse a single type of space (character, line comment, block comment)
pSpaceRaw :: Parser () -> Parser ()
pSpaceRaw p =
  [ p
  , pLineComment
  , pBlockComment ]
  |> map hidden
  |> choice

-- | Parse any amount of whitespace.
pSpace :: Parser ()
pSpace = pSpaceRaw space1 |> skipMany

-- | Parse any amount of horizontal whitespace.
pHSpace :: Parser ()
pHSpace = pSpaceRaw hspace1 |> skipMany

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

-- | Parse an integer literal.
-- It can have an optional prefix: 0b, 0o or 0x
-- for binary, octal and hexadecimal.
-- Also, underscores are ignored.
pInt :: Parser Integer
pInt =
  choice -- parse optional prefix, select char validator and base
    [ string "0b" $> (02, (`elem` ['0', '1']))
    , string "0o" $> (08, isOctDigit)
    , string "0x" $> (16, isHexDigit)
    , pure           (10, isDigit) ]
  >>= \(base, valid) -> choice -- parse a valid char or underscore
    [ satisfy valid <&> Just
    , char '_'       $> Nothing ]
  |> some               -- an integer has some chars
  |> fmap (             -- [Char] -> Integer conversion
    catMaybes           -- take only valid chars
    .> foldl' (\res ->  -- from left to right:
      digitToInt        -- interpret current char as digit
      .> toInteger
      .> (+ res * base) -- add to result, shifted by base
    ) 0
  )

-- | Parse a fraction: two integers separated by /
pFrac :: Parser Rational
pFrac = liftA2 (%) (pInt <* char '/') pInt

-- | Parse many AST nodes in a list, specified by two surrounding chars
pListRaw :: (Char, Char) -> Parser [AST]
pListRaw (l, r) = char l *> pSpace *> pAST `sepEndBy` pSpace <* char r

-- | Parse a tuple: items surrounded by ()
pTup :: Parser [AST]
pTup = pListRaw ('(', ')')

-- | Parse an array: items surrounded by []
pArr :: Parser [AST]
pArr = pListRaw ('[', ']')

-- | Parse a block: a list of lines surrounded by {}
-- A line is a tuple without the surrounding ()
-- and ends on a linebreak.
pBlk :: Parser [[AST]]
pBlk =
  char '{' *> pSpace *>
  (pAST `sepEndBy` pHSpace) `sepEndBy` pSpace
  <* char '}'
