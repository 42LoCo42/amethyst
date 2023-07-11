{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Megaparsec (MonadParsec (eof), parseTest)

import Parser

main :: IO ()
main = parseTest (pAST' (<* eof)) "~/foo/bar/baz"
