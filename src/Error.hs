module Error (KellogsError (..)) where

import Control.Monad.Except

data KellogsError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
  deriving (Show)

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue (Right val) = val