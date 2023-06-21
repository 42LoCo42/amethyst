module Types where

data AST
  = ASTSymbol Text
  | ASTChar Char
  | ASTText Text
  deriving (Show)
