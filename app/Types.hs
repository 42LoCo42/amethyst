module Types (AST (..)) where

data AST
  = ASTSymbol Text
  | ASTText Text
  deriving (Show)
