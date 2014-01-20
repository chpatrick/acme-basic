{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | With exciting new unstructed programming features, Haskell can finally see some real world use.
--
-- >main :: IO ()
-- >main = _BEGIN $ do
-- >  _10 <- _PRINT "LOOK AROUND YOU"
-- >  _20 <- _GOTO _10
-- >  _END
--

module Acme.BASIC
  ( BASIC(), LABEL(), COMMAND, END, PROGRAM
-- * Declarations
  , _BEGIN, _END
-- * Commands
  , _REM, _PRINT, _GOTO
  ) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Fix

newtype BASIC a = BASIC (ContT () IO a)
  deriving Monad

newtype LABEL = LABEL (IO ())

type COMMAND = BASIC LABEL

data END = END

type PROGRAM = BASIC END

-- | Declares a BASIC program.
_BEGIN :: PROGRAM -> IO ()
_BEGIN (BASIC c) = runContT c (const (return ()))

-- | Declares the end of a BASIC program.
_END :: BASIC END
_END = return END

-- | Terminate the BASIC session and return to the teletype console.
_STOP :: COMMAND
_STOP = BASIC $ ContT (const (return ())) >> return undefined

getc :: ContT () IO LABEL
getc = LABEL <$> ContT fix

command :: IO () -> COMMAND
command m = BASIC $ do
  c <- getc
  lift m
  return c

-- | Anything goes! This string is ignored by BASIC.
_REM :: String -> COMMAND
_REM _ = BASIC getc

-- | Print a message to the teletype console.
_PRINT :: String -> COMMAND
_PRINT = command . putStrLn

-- | Cause execution to jump to a label.
_GOTO :: LABEL -> COMMAND
_GOTO (LABEL c) = BASIC . ContT $ const c
