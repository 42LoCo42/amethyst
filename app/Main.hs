{-# LANGUAGE OverloadedStrings #-}
module Main where

import Flow            ((|>))
import Text.Megaparsec (MonadParsec (eof), Parsec, errorBundlePretty, runParser)

import Parser (pEscapedChar)

run :: (Show a) => Parsec Void Text a -> Text -> IO ()
run p t =
  runParser (p <* eof) "" t
  |> either errorBundlePretty show
  |> putStrLn

main :: IO ()
main = run pEscapedChar "\\DC3"
