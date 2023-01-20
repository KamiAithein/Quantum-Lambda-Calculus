module Main (main) where

import System.Environment
import Text.ParserCombinators.Parsec

import Common
import Parser

main :: IO ()
main = do
    toParse:_ <- getArgs
    print $ parse qlcTerm "fail?" toParse

