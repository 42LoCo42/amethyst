{-# LANGUAGE OverloadedStrings #-}
module Main where

import Flow            ((|>))
import Text.Megaparsec (MonadParsec (eof), Parsec, errorBundlePretty, runParser)

import Parser

run :: (Show a) => Parsec Void Text a -> Text -> IO ()
run p t =
  runParser (p <* eof) "" t
  |> either errorBundlePretty show
  |> putStrLn

main :: IO ()
main = run pAST "0o755_123"
