{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Mgw.Util.PriorityAgingQueue
Description : Priority Search Queue with four different prioritiy classes and support for aging.

The queue supports inserting key/value pairs with a timestamp and a
priority class set to either 'PaqRealTime', 'PaqExpress', 'PaqRegular' or
'PaqBulk'.  When dequeuing first the real-time, then the express, then the
regular and then the bulk entries are returned in order of the timestamp
given at insertion time.  If priority and timestamp are the same the
entries are returned first-in-first-out in the same order the were
enqueued.  The priority of old entries - determined by the timestamp given
1at insertion time - can be upgraded using the 'upgrade' function.  The
upgraded priority will never reach the real-time class if the entry has
not been inserted using the real-time class.  Using 'PaqUpgradeCfg' the
caller can determine how many entries may be upgraded at most and how old
the entries must be to be considered for upgrade.

The queue never contains the same key twice.  If a key is inserted twice
the associated value is upgraded, its original insertion timestamp will stay
the same and its priority will be upgraded to the higher priority class.
-}
module Control.Computations.CompEngine.Utils.PriorityAgingQueue (
  EnqueueInfo (..),
  PaqEntry (..),
  PaqKey,
  PaqPriority (..),
  PaqTime (..),
  PaqUpgradeCfg (..),
  PaqView (..),
  PriorityAgingQueue,
  delete,
  deleteView,
  dequeue,
  empty,
  enqueue,
  never,
  null,
  size,
  upgrade,
  view,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import qualified Data.Foldable as F
import qualified Data.HashPSQ as PSQ
import Data.Hashable (Hashable)
import Data.Strict.Tuple (Pair (..), (:!:))
import qualified Data.Strict.Tuple as S
import Data.LargeHashable
import Data.Word (Word64)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), elements, frequency)
import Prelude hiding (null)

type PaqKey k = (Hashable k, Ord k)

newtype PaqTime = PaqTime {unPaqTime :: Int}
  deriving (Show, Eq, Ord, Num)

{- | Counter for insertion operations to keep them in order.  64-Bits is way enough to keep our
 queue running for years.
-}
newtype PaqCounter = PaqCounter {unPaqCounter :: Word64}
  deriving (Eq, Ord, Show)

emptyCounter :: PaqCounter
emptyCounter = PaqCounter 0

never :: PaqTime
never = PaqTime maxBound

{- | The 'PaqPrioIndex' determines the order in which the entries of one
 priority class will be dequeued from the respective queue. The key with
 the smallest time, the highest original insertion priority and smallest
 insertion counter will be dequeued first.
-}
data PaqPrioIndex = PaqPrioIndex
  { _paqi_time :: PaqTime
  , _paqi_priority :: PaqPriority
  , _paqi_counter :: PaqCounter
  }
  deriving (Eq, Ord, Show)

{- | Lazy view of all entries in order of priority.  This structure is lazy so you don't pay
 for entries you don't want to look at.
-}
data PaqView k v = PaqView
  { paqv_realTimeQueue :: ~[PaqEntry k v]
  , paqv_expressQueue :: ~[PaqEntry k v]
  , paqv_regularQueue :: ~[PaqEntry k v]
  , paqv_bulkQueue :: ~[PaqEntry k v]
  }
  deriving (Eq, Show)

type PaqQueue k v = PSQ.HashPSQ k PaqPrioIndex v

-- | Every field explicitly strict. Correction to an earlier version of this
-- comment: this project builds with @-XStrictData@ on by default
-- (package.yaml's @default-extensions@), so every field here -- including
-- the four sub-queues and the counter -- was /already/ forced to WHNF on
-- construction even before the explicit @!@\/'paq_size''s own bang were
-- added; there was no latent laziness bug in this record itself (confirmed
-- empirically: this project's A/B benchmark for the strictness-audit change
-- that added these explicit bangs showed no measurable movement). The
-- explicit @!@ is kept anyway as a documentation/robustness belt-and-
-- suspenders measure -- it makes the invariant survive a hypothetical future
-- @NoStrictData@ pragma on this module rather than depending silently on a
-- package-wide default a reader of this file alone wouldn't see. The one
-- genuine gap this module's write side had was in 'SimpleStateIf.hs'\'s
-- @sifs_stale@ write site: 'enqueue' (unlike 'dequeue'\/'deleteView', whose
-- results are pattern-matched through the strict @:!:@ pair) returns a plain
-- lazy tuple, and @let (how, q') = ...@ leaves @q'@ an unforced *selector
-- thunk* regardless of how strict 'PriorityAgingQueue'\'s own fields are --
-- that thunk was the real fix (see 'colWrite''s haddock in
-- "Utils/DefTable.hs" for the general rationale, and @sifs_stale@'s own
-- write site for this specific one).
data PriorityAgingQueue k v = PriorityAgingQueue
  { paq_nextCounter :: {-# NOUNPACK #-} !PaqCounter
  -- Total entry count across all four sub-queues, maintained incrementally
  -- by every operation that adds or removes a key (enqueue/deleteView/
  -- dequeue -- upgrade only moves entries between sub-queues, so it never
  -- touches this). Exists purely so 'size' is O(1): the underlying
  -- Data.HashPSQ.size is O(n) (it folds the whole tree), and 'size' used to
  -- be called from there directly -- once per dequeued cap on the engine's
  -- hot per-rerun path (Control.Computations.CompEngine.Impl's
  -- stepCompEngine) -- an O(queue size) cost paid on every single rerun,
  -- i.e. quadratic in the size of a propagation round. See
  -- docs/benchmark-notes.md's live-update investigation for the
  -- measurement that caught this.
  , paq_size :: {-# UNPACK #-} !Int
  , paq_realTimeQueue :: {-# NOUNPACK #-} !(PaqQueue k v)
  , paq_expressQueue :: {-# NOUNPACK #-} !(PaqQueue k v)
  , paq_regularQueue :: {-# NOUNPACK #-} !(PaqQueue k v)
  , paq_bulkQueue :: {-# NOUNPACK #-} !(PaqQueue k v)
  }

instance (Show k, PaqKey k) => Show (PriorityAgingQueue k v) where
  showsPrec p (view -> PaqView rtq xq rq bq) =
    showParen (p >= 10) $
      showString "PriorityAgingQueue.fromView "
        . showsPrec 10 (map paqe_key rtq)
        . showString " "
        . showsPrec 10 (map paqe_key xq)
        . showString " "
        . showsPrec 10 (map paqe_key rq)
        . showString " "
        . showsPrec 10 (map paqe_key bq)

data PaqPriority
  = -- | will be served first no matter what
    PaqRealTime
  | -- | will be served after real-time work
    PaqExpress
  | -- | will be served after express work
    PaqRegular
  | -- | will be served after regular work
    PaqBulk
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance Hashable PaqPriority

data PaqUpgradeCfg = PaqUpgradeCfg
  { paqu_bulkToRegularTime :: PaqTime
  -- ^ After this time bulk work will be upgraded to regular work.
  , paqu_regularToExpressTime :: PaqTime
  -- ^ After this time regular work will be upgraded to express work.
  , paqu_bulkToExpressTime :: PaqTime
  }

data PaqEntry k v = PaqEntry
  { paqe_insertionTime :: PaqTime
  , paqe_insertionPriority :: PaqPriority
  , paqe_key :: k
  , paqe_value :: v
  }
  deriving (Eq, Show)

empty :: PriorityAgingQueue k v
empty = PriorityAgingQueue emptyCounter 0 PSQ.empty PSQ.empty PSQ.empty PSQ.empty

view :: PaqKey k => PriorityAgingQueue k v -> PaqView k v
view paq =
  PaqView
    { paqv_bulkQueue = fst (takeMinWhile (const True) (paq_bulkQueue paq))
    , paqv_regularQueue = fst (takeMinWhile (const True) (paq_regularQueue paq))
    , paqv_expressQueue = fst (takeMinWhile (const True) (paq_expressQueue paq))
    , paqv_realTimeQueue = fst (takeMinWhile (const True) (paq_realTimeQueue paq))
    }

getQueues :: PriorityAgingQueue k v -> [PaqQueue k v]
getQueues x =
  [ paq_realTimeQueue x
  , paq_expressQueue x
  , paq_regularQueue x
  , paq_bulkQueue x
  ]

setQueues :: PriorityAgingQueue k v -> [PaqQueue k v -> PriorityAgingQueue k v]
setQueues paq =
  [ \q -> paq{paq_realTimeQueue = q}
  , \q -> paq{paq_expressQueue = q}
  , \q -> paq{paq_regularQueue = q}
  , \q -> paq{paq_bulkQueue = q}
  ]

loopQs
  :: a
  -> ((PaqQueue k v, PaqQueue k v -> PriorityAgingQueue k v) -> a -> a)
  -> PriorityAgingQueue k v
  -> a
loopQs def f paq = loop (zip (getQueues paq) (setQueues paq))
 where
  loop qqs =
    case qqs of
      (q : qs) -> f q (loop qs)
      [] -> def

-- | /O(1)/ -- see 'paq_size'\'s haddock for why this used to be /O(n)/ and
-- why that mattered.
size :: PriorityAgingQueue k v -> Int
size = paq_size

null :: PriorityAgingQueue k v -> Bool
null = all PSQ.null . getQueues

delete :: PaqKey k => k -> PriorityAgingQueue k v -> PriorityAgingQueue k v
delete k paq = option paq S.snd (deleteView k paq)

-- | Decrements 'paq_size' on a hit -- 'setQ' (from 'loopQs'\/'setQueues')
-- is a plain record update of one sub-queue field, so it carries the
-- pre-deletion 'paq_size' through unchanged; this is the one place that
-- actually removes an entry, so it's also the one place that has to
-- account for it.
deleteView
  :: PaqKey k
  => k
  -> PriorityAgingQueue k v
  -> Option (PaqEntry k v :!: PriorityAgingQueue k v)
deleteView k =
  loopQs None $ \(q, setQ) loop ->
    case PSQ.deleteView k q of
      Just (PaqPrioIndex t p _, v, q') ->
        let paq' = setQ q' in Some (PaqEntry t p k v :!: paq'{paq_size = paq_size paq' - 1})
      Nothing -> loop

dequeue :: PaqKey k => PriorityAgingQueue k v -> (Option (PaqEntry k v :!: PriorityAgingQueue k v))
dequeue =
  loopQs None $ \(q, setQ) loop ->
    case PSQ.minView q of
      Just (k, PaqPrioIndex t p _, v, q') ->
        let paq' = setQ q' in Some (PaqEntry t p k v :!: paq'{paq_size = paq_size paq' - 1})
      Nothing -> loop

data EnqueueInfo
  = EnqueueAddedNewEntry
  | EnqueueUpdatedEntry
  deriving (Show, Eq, Generic)

instance Hashable EnqueueInfo

enqueue
  :: PaqKey k
  => PaqEntry k v
  -> PriorityAgingQueue k v
  -> (EnqueueInfo, PriorityAgingQueue k v)
enqueue e@(PaqEntry t1 p1 k v) paq =
  case deleteView k paq of
    None ->
      (EnqueueAddedNewEntry, enqueueNew e paq)
    Some (PaqEntry t0 p0 _ _ :!: paq') ->
      (EnqueueUpdatedEntry, enqueueNew (PaqEntry (min t0 t1) (min p0 p1) k v) paq')
 where
  -- +1 unconditionally: called either on the never-deleted original 'paq'
  -- (genuinely new key, 0 -> 1) or on 'deleteView'\'s already-decremented
  -- result (same key replaced, net 0) -- both correct by construction, no
  -- need to case on 'EnqueueAddedNewEntry'\/'EnqueueUpdatedEntry' here.
  enqueueNew (PaqEntry t p k v) paq =
    case p of
      PaqRealTime -> paq'{paq_realTimeQueue = PSQ.insert k pi v (paq_realTimeQueue paq)}
      PaqExpress -> paq'{paq_expressQueue = PSQ.insert k pi v (paq_expressQueue paq)}
      PaqRegular -> paq'{paq_regularQueue = PSQ.insert k pi v (paq_regularQueue paq)}
      PaqBulk -> paq'{paq_bulkQueue = PSQ.insert k pi v (paq_bulkQueue paq)}
   where
    paq' = paq{paq_nextCounter = nextCounter (paq_nextCounter paq), paq_size = paq_size paq + 1}
    pi = PaqPrioIndex t p (paq_nextCounter paq)

nextCounter :: PaqCounter -> PaqCounter
nextCounter c = PaqCounter (unPaqCounter c + 1)

-- | Upgrades old work to higher priority queues according to the given 'UpgradeCfg'.
upgrade :: forall k v. PaqKey k => PaqUpgradeCfg -> PriorityAgingQueue k v -> PriorityAgingQueue k v
-- 'upgrade' only moves entries between sub-queues (dequeueOld/enqueueOld
-- below insert straight into a raw 'PaqQueue', bypassing the top-level
-- 'enqueue'/'deleteView'), so 'paq_size' is untouched here -- the closing
-- record update below inherits it unchanged from 'paq'.
upgrade ucfg paq@(PriorityAgingQueue c _sz _rt xq rq bq) =
  let (bToR, bq') = dequeueOld (paqu_bulkToRegularTime ucfg) PaqBulk bq
      (rToX, rq') = dequeueOld (paqu_regularToExpressTime ucfg) PaqRegular rq
      (bToX, rq'') = dequeueOld (paqu_bulkToExpressTime ucfg) PaqBulk rq'
      (c', rq''') = enqueueOld c bToR rq''
      (c'', xq') = enqueueOld c' rToX xq
      (c''', xq'') = enqueueOld c'' bToX xq'
   in paq
        { paq_nextCounter = c'''
        , paq_expressQueue = xq''
        , paq_regularQueue = rq'''
        , paq_bulkQueue = bq'
        }
 where
  enqueueOld :: PaqCounter -> [PaqEntry k v] -> PaqQueue k v -> (PaqCounter, PaqQueue k v)
  enqueueOld c xxs q =
    case xxs of
      [] -> (c, q)
      (PaqEntry t p k v) : xs ->
        enqueueOld (nextCounter c) xs (PSQ.insert k (PaqPrioIndex t p c) v q)
  dequeueOld :: PaqTime -> PaqPriority -> PaqQueue k v -> ([PaqEntry k v], PaqQueue k v)
  dequeueOld dqt dqp q =
    takeMinWhile (\(PaqPrioIndex t p _) -> p == dqp && t <= dqt) q

takeMinWhile :: PaqKey k => (PaqPrioIndex -> Bool) -> PaqQueue k v -> ([PaqEntry k v], PaqQueue k v)
takeMinWhile pred q =
  case PSQ.minView q of
    Just (k, pi@(PaqPrioIndex t p _), v, q')
      | pred pi ->
          let (xs, q'') = takeMinWhile pred q'
           in (PaqEntry t p k v : xs, q'')
    _ ->
      ([], q)

data PaqAction k v
  = PaqInsertAction (PaqEntry k v)
  | PaqDeleteAction k

instance (PaqKey k, Arbitrary k, Arbitrary v) => Arbitrary (PriorityAgingQueue k v) where
  arbitrary =
    do
      (ops :: [PaqAction k v]) <- arbitrary
      return $! S.fst $! F.foldl' applyAction (empty :!: PaqTime 0) ops
   where
    applyAction (paq :!: t0) action =
      case action of
        PaqInsertAction (PaqEntry ((PaqTime . (unPaqTime t0 +) . unPaqTime) -> t1) p k v) ->
          (snd (enqueue (PaqEntry t1 p k v) paq) :!: t1)
        PaqDeleteAction k ->
          (delete k paq :!: t0)

instance Arbitrary PaqPriority where
  arbitrary = elements [minBound .. maxBound]

instance Arbitrary PaqTime where
  -- NOTE: This instance is also used to represent time differences!
  arbitrary = PaqTime <$> elements [0 .. 3]

instance (Arbitrary k, Arbitrary v) => Arbitrary (PaqEntry k v) where
  -- NOTE: The paqe_insertionTime is interpreted as a time difference to a previous time
  arbitrary = PaqEntry <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary k, Arbitrary v) => Arbitrary (PaqAction k v) where
  arbitrary =
    frequency
      [ (2, PaqInsertAction <$> arbitrary)
      , (1, PaqDeleteAction <$> arbitrary)
      ]

data Key
  = KeyA
  | KeyB
  | KeyC
  | KeyD
  deriving (Read, Show, Eq, Ord, Generic)

instance Hashable Key

instance Arbitrary Key where
  arbitrary = elements [KeyA, KeyB, KeyC, KeyD]

$(deriveLargeHashable ''PaqTime)
$(deriveLargeHashable ''Key)
$(deriveLargeHashable ''PaqPriority)
$(deriveLargeHashable ''PaqCounter)
$(deriveLargeHashable ''PaqPrioIndex)
$(deriveLargeHashable ''PaqEntry)
