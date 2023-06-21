{-# LANGUAGE OverloadedStrings #-}
module Main where

import Flow            ((|>))
import Text.Megaparsec (MonadParsec (eof), Parsec, errorBundlePretty, runParser)

import Parser (pSymbol)

run :: (Show a) => Parsec Void Text a -> Text -> IO ()
run p t =
  runParser (p <* eof) "" t
  |> either errorBundlePretty show
  |> putStrLn

main :: IO ()
main = run pSymbol "A0b1c2-def\\/ghi"
