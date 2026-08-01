module Control.Computations.CompEngine (
  module X,
) where

import Control.Computations.CompEngine.CacheBehaviors as X
import Control.Computations.CompEngine.CompDef as X
import Control.Computations.CompEngine.CompEval as X
import Control.Computations.CompEngine.CompFlow as X

-- 'CompSrcInstIx'\/'withTypedCompSrcIdIndexed' are hidden here: they're
-- "Control.Computations.CompEngine.Impl"'s own interning plumbing for
-- keying a 'CompReqCombined' batch's per-instance source-group table
-- cheaply (see 'CompSrcInstIx'\'s haddock), not something a comp body or a
-- 'CompSrc' instance ever needs to see. Everything else
-- "CompFlowRegistry" exports is passed through unchanged.
import Control.Computations.CompEngine.CompFlowRegistry as X hiding (
  CompSrcInstIx,
  withTypedCompSrcIdIndexed,
 )

-- 'compSinkId'\/'compSrcId' are hidden here: they collapse a live instance
-- straight to its untyped id, which is exactly the kind of unchecked
-- shortcut this narrow public facade doesn't want to encourage. They stay
-- exported from the underlying (unexposed) modules for the library's own
-- internal use (logging, 'CompFlowRegistry' hash-map keys); external code
-- should reach for 'typedCompSinkIdOf'\/'typedCompSrcIdOf' and keep the
-- typed id, or apply 'unTypedCompSinkId'\/'unTypedCompSrcId' itself if the
-- untyped form is genuinely what's needed.
--
-- 'wrapCompSrcDepWithId' is hidden for the same reason: it trusts its
-- caller to pass exactly the given instance's own 'CompSrcId', with no
-- check that the two actually match (that's the whole point -- see its
-- haddock). It exists purely for "Control.Computations.CompEngine.Impl"'s
-- hot paths, which already have that id in hand from a registry lookup;
-- external code should keep using 'wrapCompSrcDep'.
import Control.Computations.CompEngine.CompSink as X hiding (compSinkId)
import Control.Computations.CompEngine.CompSrc as X hiding (
  compSrcId,
  wrapCompSrcDepWithId,
 )
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
-- (see Types.hs), so exposing its constructor/'runCompM' accessor through
-- this public facade would let a computation body construct a 'CompM'
-- value by hand and smuggle arbitrary IO into the engine. 'CompMEnv' (the
-- env that constructor would need) is hidden entirely for the same
-- reason. 'CompReqLeaf'\/'traverseCompReq' are hidden entirely too -- they
-- are "Control.Computations.CompEngine.Impl"'s own leaf-preparation
-- plumbing for batched 'CompReqCombined' requests, of no use to a comp
-- body. Everything else Types.hs exports is passed through unchanged.
import Control.Computations.CompEngine.Types as X hiding (
  CompM (..),
  CompMEnv (..),
  CompReqLeaf (..),
  traverseCompReq,
 )
import Control.Computations.CompEngine.Types as X (CompM)
