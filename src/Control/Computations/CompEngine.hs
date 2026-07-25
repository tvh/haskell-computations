module Control.Computations.CompEngine (
  module X,
) where

import Control.Computations.CompEngine.CacheBehaviors as X
import Control.Computations.CompEngine.CompDef as X
import Control.Computations.CompEngine.CompEval as X
import Control.Computations.CompEngine.CompFlow as X
import Control.Computations.CompEngine.CompFlowRegistry as X
import Control.Computations.CompEngine.CompSink as X
import Control.Computations.CompEngine.CompSrc as X
import Control.Computations.CompEngine.Core as X
import Control.Computations.CompEngine.Driver as X
import Control.Computations.CompEngine.Run as X

-- 'PaqPriority' is defined in the (hidden) columnar state layer's
-- "Control.Computations.CompEngine.Utils.PriorityAgingQueue", but it shows
-- up in this facade's own public surface -- 'defineCompWithPriority',
-- 'compId_priority', 'mkCompIdWithPriority' and 'anyCompApPriority' (all
-- re-exported below via CompDef/Types) take or return it -- so it must be
-- reachable. Re-exporting just the one type here is cheaper than exposing
-- the whole 395-line priority-queue module for four constructors.
import Control.Computations.CompEngine.Utils.PriorityAgingQueue as X (PaqPriority (..))

-- 'CompM' is re-exported *abstractly* here: its representation wraps IO
-- (see Types.hs), so unlike the old pure representation, exposing its
-- constructor/'runCompM' accessor through this public facade would let a
-- computation body construct a 'CompM' value by hand and smuggle arbitrary
-- IO into the engine. 'CompMEnv' (the env that constructor would need) is
-- hidden entirely for the same reason. Everything else Types.hs exports is
-- passed through unchanged.
import Control.Computations.CompEngine.Types as X hiding (CompM (..), CompMEnv (..))
import Control.Computations.CompEngine.Types as X (CompM)
