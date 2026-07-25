{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.CompEngine.Utils.PriorityAgingQueueTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.Utils.PriorityAgingQueue
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Strict.Tuple (Pair (..))
import Test.Framework
import Prelude hiding (null)

t0, t5, t10 :: PaqTime
t0 = PaqTime 0
t5 = PaqTime 5
t10 = PaqTime 10

test_enqueueTwoElementsWithSamePriority :: IO ()
test_enqueueTwoElementsWithSamePriority =
  do
    let foo = PaqEntry t0 PaqRegular "foo" () :: PaqEntry String ()
        bar = PaqEntry t0 PaqRegular "bar" () :: PaqEntry String ()
        (ei1, q1) = enqueue foo empty
        (ei2, q2) = enqueue bar q1
    assertEqual (PaqView [] [] [foo, bar] []) (view q2)
    assertEqual EnqueueAddedNewEntry ei1
    assertEqual EnqueueAddedNewEntry ei2

test_dequeueOneOfTwoElementsWithSamePriority :: IO ()
test_dequeueOneOfTwoElementsWithSamePriority =
  do
    let foo = PaqEntry t0 PaqRegular "foo" () :: PaqEntry String ()
        bar = PaqEntry t0 PaqRegular "bar" () :: PaqEntry String ()
        (_, q1) = enqueue foo empty
        (_, q2) = enqueue bar q1
        dq2 = optionToMaybe (dequeue q2)
    (edq2 :!: qdq2) <- assertJust dq2
    assertEqual foo edq2
    assertEqual (PaqView [] [] [bar] []) (view qdq2)

test_dequeueOneOfTwoElementsWithSamePriorityReverse :: IO ()
test_dequeueOneOfTwoElementsWithSamePriorityReverse =
  do
    -- test that insertion order is maintained even if time is the same
    let foo = PaqEntry t0 PaqRegular "foo" () :: PaqEntry String ()
        bar = PaqEntry t0 PaqRegular "bar" () :: PaqEntry String ()
        (_, q1) = enqueue bar empty
        (_, q2) = enqueue foo q1
        dq2 = optionToMaybe (dequeue q2)
    (edq2 :!: qdq2) <- assertJust dq2
    assertEqual bar edq2
    assertEqual (PaqView [] [] [foo] []) (view qdq2)

test_dequeueOneOfTwoElementsWithSamePriorityWhereGivenInsertionTimeMattersNotInsertionOrder :: IO ()
test_dequeueOneOfTwoElementsWithSamePriorityWhereGivenInsertionTimeMattersNotInsertionOrder =
  do
    let foo = PaqEntry t5 PaqRegular "foo" () :: PaqEntry String ()
        bar = PaqEntry t0 PaqRegular "bar" () :: PaqEntry String ()
        (_, q1) = enqueue foo empty
        (_, q2) = enqueue bar q1
        dq2 = optionToMaybe (dequeue q2)
    (edq2 :!: qdq2) <- assertJust dq2
    assertEqual bar edq2
    assertEqual (PaqView [] [] [foo] []) (view qdq2)

test_dequeueOneOfTwoElementsWithRegularAndExpressPriority :: IO ()
test_dequeueOneOfTwoElementsWithRegularAndExpressPriority =
  do
    let foo = PaqEntry t0 PaqRegular "foo" () :: PaqEntry String ()
        bar = PaqEntry t0 PaqExpress "bar" () :: PaqEntry String ()
        (_, q1) = enqueue foo empty
        (_, q2) = enqueue bar q1
        dq2 = optionToMaybe (dequeue q2)
    (edq2 :!: qdq2) <- assertJust dq2
    assertEqual bar edq2
    assertEqual (PaqView [] [] [foo] []) (view qdq2)

test_dequeueOneOfTwoElementsWithDifferentPriorities :: IO ()
test_dequeueOneOfTwoElementsWithDifferentPriorities =
  do
    let foo = PaqEntry t0 PaqExpress "foo" () :: PaqEntry String ()
        bar = PaqEntry t5 PaqRealTime "bar" () :: PaqEntry String ()
        (_, q1) = enqueue foo empty
        (_, q2) = enqueue bar q1
        dq2 = optionToMaybe (dequeue q2)
    (edq2 :!: qdq2) <- assertJust dq2
    assertEqual bar edq2
    assertEqual (PaqView [] [foo] [] []) (view qdq2)

test_enqueingSameKeyTwiceIncreasesPriority :: IO ()
test_enqueingSameKeyTwiceIncreasesPriority =
  do
    let foo1 = PaqEntry t0 PaqRegular "foo" True :: PaqEntry String Bool
        foo2 = PaqEntry t5 PaqExpress "foo" False :: PaqEntry String Bool
        (ei1, q1) = enqueue foo1 empty
        (ei2, q2) = enqueue foo2 q1
    assertEqual (PaqView [] [PaqEntry t0 PaqExpress "foo" False] [] []) (view q2)
    assertEqual EnqueueAddedNewEntry ei1
    assertEqual EnqueueUpdatedEntry ei2

test_upgradeRegularToExpressOvertakingNewerExpressWork :: IO ()
test_upgradeRegularToExpressOvertakingNewerExpressWork =
  do
    let foo = PaqEntry t0 PaqRegular "foo" () :: PaqEntry String ()
        bar = PaqEntry t5 PaqExpress "bar" () :: PaqEntry String ()
        (_, q1) = enqueue foo empty
        (_, q2) = enqueue bar q1
        q3 = upgrade ucfg q2
        dq3 = optionToMaybe (dequeue q3)
    (edq3 :!: qdq3) <- assertJust dq3
    assertEqual foo edq3
    assertEqual (PaqView [] [bar] [] []) (view qdq3)
 where
  ucfg =
    PaqUpgradeCfg
      { paqu_regularToExpressTime = t5
      , paqu_bulkToRegularTime = never
      , paqu_bulkToExpressTime = never
      }

test_upgradeRegularToExpressNotOvertakingOlderExpressWork :: IO ()
test_upgradeRegularToExpressNotOvertakingOlderExpressWork =
  do
    let foo = PaqEntry t0 PaqExpress "foo" () :: PaqEntry String ()
        bar = PaqEntry t5 PaqRegular "bar" () :: PaqEntry String ()
        (_, q1) = enqueue foo empty
        (_, q2) = enqueue bar q1
        q3 = upgrade ucfg q2
        dq3 = optionToMaybe (dequeue q3)
    (edq3 :!: qdq3) <- assertJust dq3
    assertEqual foo edq3
    assertEqual (PaqView [] [bar] [] []) (view qdq3)
 where
  ucfg =
    PaqUpgradeCfg
      { paqu_regularToExpressTime = t5
      , paqu_bulkToRegularTime = never
      , paqu_bulkToExpressTime = never
      }

test_upgradeMaxCountFromRegularToExpress :: IO ()
test_upgradeMaxCountFromRegularToExpress =
  do
    let foo = PaqEntry t5 PaqRegular "foo" () :: PaqEntry String ()
        bar = PaqEntry t0 PaqRegular "bar" () :: PaqEntry String ()
        baz = PaqEntry t10 PaqExpress "baz" () :: PaqEntry String ()
        (_, q1) = enqueue foo empty
        (_, q2) = enqueue bar q1
        q3 = upgrade (ucfg t0) q2
        dq3 = optionToMaybe (dequeue q3)
    (edq3 :!: qdq3) <- assertJust dq3
    assertEqual bar edq3
    assertEqual (PaqView [] [] [foo] []) (view qdq3)
    let (_, q4) = enqueue baz qdq3
        q5 = upgrade (ucfg t10) q4
        dq5 = optionToMaybe (dequeue q5)
    (edq5 :!: qdq5) <- assertJust dq5
    assertEqual foo edq5
    assertEqual (PaqView [] [baz] [] []) (view qdq5)
 where
  ucfg t =
    PaqUpgradeCfg
      { paqu_regularToExpressTime = t
      , paqu_bulkToRegularTime = never
      , paqu_bulkToExpressTime = never
      }
