{-# LANGUAGE TypeFamilies #-}

{- | A 'Control.Computations.CompEngine.CompSrc.CompSrc' that produces the
 current time, truncated to a chosen 'TimeIntervalType' granularity, and
 wakes up dependents once per truncated interval. This is how a comp gets a
 periodic tick without polling: request the time at a given granularity
 (with 'compGetTime', against 'defaultTimeSrcId'), and the engine reruns
 that comp whenever the truncated time value changes. Wire it in with
 'withDefaultTimeSrc' (or 'withTimeSrc' for a non-default instance id\/clock).
-}
module Control.Computations.FlowImpls.TimeSrc (
  TimeSrcReq (..),
  TimeSrc,
  TimeKey (..),
  TimeVer (..),
  initTimeSrc,
  closeTimeSrc,
  withTimeSrc,
  defaultTimeSrcId,
  withDefaultTimeSrc,
  compGetTime,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.Clock
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.TimeUtils
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.LargeHashable as LH
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Time.Clock

-- | A request against a 'TimeSrc': the current time truncated to the given
-- granularity.
data TimeSrcReq a where
  GetTime :: TimeIntervalType -> TimeSrcReq UTCTime

-- | Identifies a tracked granularity ('TimeIntervalType') within a 'TimeSrc'.
newtype TimeKey = TimeKey {unTimeKey :: TimeIntervalType}
  deriving (Eq, Ord, Show, Hashable, LH.LargeHashable)

-- | The truncated time value last observed for a given 'TimeKey'.
newtype TimeVer = TimeVer {unTimeVer :: UTCTime}
  deriving (Eq, Ord, Show, Hashable, LH.LargeHashable)

type TimeDep = Dep TimeKey TimeVer

-- | A running time source handle, holding the background ticking thread.
-- Register it with a 'Control.Computations.CompEngine.CompFlowRegistry.CompFlowRegistry'
-- to make it available to comp bodies.
data TimeSrc = TimeSrc
  { tcs_ident :: CompSrcInstanceId
  , tcs_thread :: Async ()
  , tcs_truncatedTimes :: TVar (TimeIntervalType -> UTCTime)
  , tcs_state :: TVar (Map TimeIntervalType UTCTime)
  }

defaultTimeSrcInstanceId :: CompSrcInstanceId
defaultTimeSrcInstanceId = CompSrcInstanceId "defaultTimeSrc"

-- | The typed source id of the 'TimeSrc' registered by 'withDefaultTimeSrc';
-- pass this to 'compGetTime' if you need the raw request/id pair instead.
--
-- Necessarily a top-level 'unsafeMkTypedCompSrcId': this id is part of the
-- public API and gets baked into comp bodies (via 'compGetTime') long
-- before any 'TimeSrc' instance exists, so there is no live instance to
-- derive it from with 'typedCompSrcIdOf'. It shares
-- 'defaultTimeSrcInstanceId' with 'withDefaultTimeSrc', which is what
-- actually constructs the instance, so the two names can't drift apart.
defaultTimeSrcId :: TypedCompSrcId TimeSrc
defaultTimeSrcId = unsafeMkTypedCompSrcId (Proxy @TimeSrc) defaultTimeSrcInstanceId

-- | Request the current time, truncated to the given granularity, from the
-- default 'TimeSrc' (see 'withDefaultTimeSrc'). The comp calling this reruns
-- whenever the truncated value next changes.
compGetTime :: TimeIntervalType -> CompM UTCTime
compGetTime t = compSrcReq defaultTimeSrcId (GetTime t)

-- | Start a 'TimeSrc' with the given source id and clock. Prefer
-- 'withTimeSrc' (or 'withDefaultTimeSrc') unless you need to manage its
-- lifetime by hand.
initTimeSrc :: CompSrcInstanceId -> Clock -> IO TimeSrc
initTimeSrc ident clock = do
  startTimes <- getTruncatedTimes
  timesVar <- newTVarIO startTimes
  lastStateVar <- newTVarIO Map.empty
  thread <- async (loop timesVar startTimes)
  pure (TimeSrc ident thread timesVar lastStateVar)
 where
  loop timesVar curTimes = do
    newTimes <- getTruncatedTimes
    unless (all (\ti -> newTimes ti == curTimes ti) [minBound .. maxBound]) $
      atomically (writeTVar timesVar newTimes)
    let now = newTimes TimeInterval1s
    c_sleepUntil clock (now `addTimeSpan` seconds 1)
    loop timesVar newTimes
  getTruncatedTimes = do
    t <- c_currentTime clock
    pure (truncateTime t)

-- | Stop a 'TimeSrc' started with 'initTimeSrc', cancelling its background
-- ticking thread.
closeTimeSrc :: TimeSrc -> IO ()
closeTimeSrc tcs = cancel (tcs_thread tcs)

-- | Start a 'TimeSrc', run @action@, and stop it afterwards even if
-- @action@ throws.
withTimeSrc :: CompSrcInstanceId -> Clock -> (TimeSrc -> IO a) -> IO a
withTimeSrc ident clock =
  bracket (initTimeSrc ident clock) closeTimeSrc

-- | 'withTimeSrc' with the default source id (the one 'compGetTime' and
-- 'defaultTimeSrcId' assume) and 'realClock'. This is what most programs
-- want.
withDefaultTimeSrc :: (TimeSrc -> IO a) -> IO a
withDefaultTimeSrc = withTimeSrc defaultTimeSrcInstanceId realClock

instance CompSrc TimeSrc where
  type CompSrcReq TimeSrc = TimeSrcReq
  type CompSrcKey TimeSrc = TimeKey
  type CompSrcVer TimeSrc = TimeVer
  compSrcInstanceId = tcs_ident
  compSrcExecute = executeImpl
  compSrcUnregister = unregisterImpl
  compSrcWaitChanges = waitChangesImpl

waitChangesImpl :: TimeSrc -> STM (HashSet TimeDep)
waitChangesImpl tcs = do
  truncTimes <- readTVar (tcs_truncatedTimes tcs)
  lastState <- readTVar (tcs_state tcs)
  let outdated =
        flip Map.mapMaybeWithKey lastState $ \interval old ->
          let new = truncTimes interval
           in if new == old then Nothing else Just new
  when (Map.null outdated) retry
  let newState = Map.unionWith max outdated lastState
      changedDeps =
        HashSet.fromList $
          map (\(k, v) -> Dep (TimeKey k) (TimeVer v)) (Map.toList outdated)
  writeTVar (tcs_state tcs) newState
  pure changedDeps

executeImpl :: TimeSrc -> TimeSrcReq a -> IO (HashSet TimeDep, Fail a)
executeImpl tcs (GetTime interval) = atomically $ do
  timesFun <- readTVar (tcs_truncatedTimes tcs)
  let time = timesFun interval
      dep = Dep (TimeKey interval) (TimeVer time)
  modifyTVar' (tcs_state tcs) (Map.insertWith min interval time)
  pure (HashSet.singleton dep, Ok time)

unregisterImpl :: TimeSrc -> HashSet TimeKey -> IO ()
unregisterImpl tcs keys = atomically $ do
  forM_ keys $ \interval -> modifyTVar' (tcs_state tcs) (Map.delete (unTimeKey interval))
