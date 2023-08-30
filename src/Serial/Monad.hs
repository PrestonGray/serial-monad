module Serial.Monad where

import qualified Control.Concurrent.Async as Async
import Data.Bifunctor (second)
import qualified Serial.Monad.Log as Log

-- | Monad for serial communication
newtype SerialMonad a = SerialMonad (CommunicationEnv -> SerialState -> IO (SerialState, Either Log.SerialMonadError a))

instance Functor SerialMonad where
  fmap f (SerialMonad a) =
    SerialMonad $ \commEnv s0 -> second (fmap f) <$> a commEnv s0

-- | TODO: There must be a more elegant way to compose these actions
instance Applicative SerialMonad where
  pure smResult = SerialMonad $ \_ s0 -> pure (s0, Right smResult)
  {-# INLINE pure #-}
  SerialMonad f1 <*> SerialMonad f2 = SerialMonad $ \commEnv s0 -> do
    (s1, eResult) <- f1 commEnv s0
    case eResult of
      Left smError -> pure (s1, Left smError)
      Right smResult -> do
        (s2, eResult') <- f2 commEnv s1
        case eResult' of
          Left smError -> pure (s2, Left smError)
          Right smResult' -> pure (s2, Right $ smResult smResult')
  {-# INLINE (<*>) #-}

instance Monad SerialMonad where
  SerialMonad f >>= g = SerialMonad $ \commEnv s0 -> do
    (s1, eResult) <- f commEnv state
    case eResult of
      Left smError -> pure (s1, Left smError)
      Right smResult ->
        let SerialMonad h = g smResult
         in h commEnv s1

instance MonadFail SerialMonad where
  fail = throwError . Log.MonadFail

--------------------
-- Error Handling --
--------------------

-- | Cancels SerialLink's async action with an exception
sendSerialMonadError :: SerialLink a -> Log.SerialMonadError -> IO ()
sendSerialMonadError = Async.cancelWith . serialLinkAsync

abortSerialMonad :: SerialMonad a
abortSerialMonad = throwError Log.Abort

throwError :: Log.SerialMonadError -> SerialMonad a
throwError smError = SerialMonad $ \_ state -> pure (state, Left smError)
