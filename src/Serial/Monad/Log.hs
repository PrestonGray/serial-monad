module Serial.Monad.Log where

import Control.Exception (Exception, SomeException)

-- | Errors that can occur while running SerialMonad
data SerialMonadError
  = Abort
  | Fail String
  | ParseError String
  | IOError SomeException
  | InvalidArgument String
  deriving (Show)

instance Exception SerialMonadError
