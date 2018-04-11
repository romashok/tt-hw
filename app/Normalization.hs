module Main where

import           Control.Monad      (guard, unless)
import           System.Environment (getArgs)
import           Text.Parsec        (parse)

import           Expr
import           Reduction


main :: IO ()
main = do
    args <- getArgs
    unless (validateInput args) $ putStrLn "usage: stack exec norm <input file>"
    guard $ validateInput args

    let fileName = head args
    line <- readFile fileName
    putStrLn $ handle line
  where
    validateInput = (==1) . length
    handle line = case parse expressionParser "" line of
                      Left parseError -> show parseError
                      Right expr      -> show $ normalize expr
