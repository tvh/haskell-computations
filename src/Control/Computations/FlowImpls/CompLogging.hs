{-# LANGUAGE TypeFamilies #-}

{- | "Control.Computations.Utils.Logging"'s @log*@ functions, lifted into
 'CompM' so a comp body can log without leaving the computation monad. Each
 @log*C@ function (e.g. 'logNoteC') is 'doLogC' at a fixed level, and goes
 through 'Control.Computations.FlowImpls.IOSink.unsafeCompIO' under the
 hood.
-}
module Control.Computations.FlowImpls.CompLogging (
  doLogC,
  logTraceC,
  logDebugC,
  logInfoC,
  logNoteC,
  logWarnC,
  logErrorC,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine.Types
import Control.Computations.FlowImpls.IOSink
import Control.Computations.Utils.Logging

----------------------------------------
-- EXTERNAL
----------------------------------------
import GHC.Stack

-- | Log a message at the given level from a comp body. The @log*C@ functions
-- below are this at a fixed level with the call site's 'CallStack' filled
-- in automatically.
doLogC :: LogLevel -> CallStack -> String -> CompM ()
doLogC level stack msg = unsafeCompIO (doLog level stack msg)

logTraceC :: (HasCallStack) => String -> CompM ()
logTraceC msg = doLogC TRACE callStack msg

logDebugC :: (HasCallStack) => String -> CompM ()
logDebugC msg = doLogC DEBUG callStack msg

logInfoC :: (HasCallStack) => String -> CompM ()
logInfoC msg = doLogC INFO callStack msg

logNoteC :: (HasCallStack) => String -> CompM ()
logNoteC msg = doLogC NOTE callStack msg

logWarnC :: (HasCallStack) => String -> CompM ()
logWarnC msg = doLogC WARN callStack msg

logErrorC :: (HasCallStack) => String -> CompM ()
logErrorC msg = doLogC ERROR callStack msg
