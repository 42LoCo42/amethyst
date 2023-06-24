module Types where

data AST
  = ASTSymbol Text
  | ASTChar Char
  | ASTText Text
  | ASTInt Integer
  | ASTReal Double
  | ASTFrac Rational -- TODO decide if this should be in AST
  deriving (Show)
