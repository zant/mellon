module Main (main) where

import Eval (eval)
import Parse (readExpr)
import System.IO

repl :: IO ()
repl = do
  putStr ">> "
  hFlush stdout
  input <- getLine
  case input of
    ":q" -> putStrLn "Bye"
    _ -> (print . eval) (readExpr input) >> repl

main :: IO ()
main = do
  repl