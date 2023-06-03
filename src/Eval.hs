module Eval (eval) where

import Parse (KellogsVal (..))

eval :: KellogsVal -> KellogsVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval _ = String "Not recognised value"

apply :: String -> [KellogsVal] -> KellogsVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [KellogsVal] -> KellogsVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [KellogsVal] -> KellogsVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: KellogsVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then 0
        else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0