{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Megaparsec (MonadParsec (eof), parseTest)

import Parser

main :: IO ()
main = parseTest (pAST' (<* eof)) "0x16t"
