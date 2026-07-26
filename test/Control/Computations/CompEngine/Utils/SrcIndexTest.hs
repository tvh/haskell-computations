{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.CompEngine.Utils.SrcIndexTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompFlow (ForAnyCompFlow (..))
import Control.Computations.CompEngine.CompSrc (
  AnyCompSrcKey,
  AnyCompSrcVer,
  CompSrc (..),
  CompSrcId,
  CompSrcInstanceId (..),
  SomeCompSrcKey (..),
  SomeCompSrcVer (..),
  typedCompSrcIdOf,
  unTypedCompSrcId,
 )
import Control.Computations.CompEngine.Utils.DefTable (DefRef, mkDefRefUnsafe)
import Control.Computations.CompEngine.Utils.SrcIndex

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM (retry)
import Control.Monad (forM_)
import Data.Hashable (Hashable)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Test.Framework

test_keyInternInsertLookupRoundTrips :: IO ()
test_keyInternInsertLookupRoundTrips = do
  ki <- newKeyIntern
  i1 <- kiIntern ki k1
  i2 <- kiIntern ki k2
  assertBool (i1 /= i2)
  m1 <- kiLookup ki k1
  m2 <- kiLookup ki k2
  assertEqual (Just i1) m1
  assertEqual (Just i2) m2

test_keyInternInternIsIdempotent :: IO ()
test_keyInternInternIsIdempotent = do
  ki <- newKeyIntern
  i1 <- kiIntern ki k1
  i2 <- kiIntern ki k1
  assertEqual i1 i2
  c <- kiLiveCount ki
  assertEqual 1 c

test_keyInternLookupMissIsNothing :: IO ()
test_keyInternLookupMissIsNothing = do
  ki <- newKeyIntern
  m <- kiLookup ki k1
  assertEqual Nothing m

test_keyInternReleaseThenLookupIsNothing :: IO ()
test_keyInternReleaseThenLookupIsNothing = do
  ki <- newKeyIntern
  _ <- kiIntern ki k1
  kiRelease ki k1
  m <- kiLookup ki k1
  assertEqual Nothing m
  c <- kiLiveCount ki
  assertEqual 0 c

test_keyInternReleaseRecyclesId :: IO ()
test_keyInternReleaseRecyclesId = do
  ki <- newKeyIntern
  i1 <- kiIntern ki k1
  kiRelease ki k1
  i2 <- kiIntern ki k2
  -- the freed id is reused, not a fresh one
  assertEqual i1 i2
  assigned <- kiAssignedCount ki
  assertEqual 1 assigned

-- | Repeatedly intern-then-release the *same* key many times over and
-- assert the assigned-id count (which would grow unboundedly under a
-- never-recycled design) stays bounded, not proportional to the number of
-- cycles.
test_keyInternChurnStaysBounded :: IO ()
test_keyInternChurnStaysBounded = do
  ki <- newKeyIntern
  forM_ [1 .. 5000 :: Int] $ \n -> do
    let k = mkKey n
    _ <- kiIntern ki k
    kiRelease ki k
  assigned <- kiAssignedCount ki
  live <- kiLiveCount ki
  assertEqual 0 live
  assertBool (assigned <= 2) -- one id, ever, recycled every cycle

test_keyInternChurnWithOverlapStaysBounded :: IO ()
test_keyInternChurnWithOverlapStaysBounded = do
  ki <- newKeyIntern
  -- two keys alternately live at once (never both released simultaneously
  -- with none live), assigned count should still settle at a small bound
  -- rather than growing with the number of cycles.
  forM_ [1 .. 2000 :: Int] $ \n -> do
    let ka = mkKey (2 * n)
        kb = mkKey (2 * n + 1)
    _ <- kiIntern ki ka
    _ <- kiIntern ki kb
    kiRelease ki ka
    kiRelease ki kb
  assigned <- kiAssignedCount ki
  live <- kiLiveCount ki
  assertEqual 0 live
  assertBool (assigned <= 4)

-- | Test-only shorthand for building a 'DefRef' from a plain integer
-- literal, via the public 'mkDefRefUnsafe' escape hatch -- this module,
-- unlike "DefTable.hs" itself, does not have 'DefRef''s constructor in
-- scope.
dr :: Int -> DefRef
dr = mkDefRefUnsafe

test_arenaAppendAndToList :: IO ()
test_arenaAppendAndToList = do
  ska <- newSrcKeyArena
  skaAppend ska (dr 100) (mkVer 1)
  skaAppend ska (dr 200) (mkVer 2)
  xs <- skaToList ska
  assertEqual 2 (length xs)
  assertBool ((dr 100, mkVer 1) `elem` xs)
  assertBool ((dr 200, mkVer 2) `elem` xs)

test_arenaRemoveMissingIsFalse :: IO ()
test_arenaRemoveMissingIsFalse = do
  ska <- newSrcKeyArena
  skaAppend ska (dr 100) (mkVer 1)
  removed <- skaRemove ska (dr 999)
  assertBool (not removed)
  n <- skaLiveCount ska
  assertEqual 1 n

test_arenaRemoveMiddleKeepsOthers :: IO ()
test_arenaRemoveMiddleKeepsOthers = do
  ska <- newSrcKeyArena
  mapM_ (\i -> skaAppend ska (dr i) (mkVer i)) [1 .. 10 :: Int]
  removed <- skaRemove ska (dr 5)
  assertBool removed
  xs <- skaToList ska
  assertEqual 9 (length xs)
  assertBool ((dr 5, mkVer 5) `notElem` xs)
  mapM_ (\i -> assertBool ((dr i, mkVer i) `elem` xs)) ([1 .. 4] ++ [6 .. 10] :: [Int])

test_arenaRemoveLastEntry :: IO ()
test_arenaRemoveLastEntry = do
  ska <- newSrcKeyArena
  skaAppend ska (dr 1) (mkVer 1)
  removed <- skaRemove ska (dr 1)
  assertBool removed
  empty <- skaNull ska
  assertBool empty

test_arenaEmptyIsNull :: IO ()
test_arenaEmptyIsNull = do
  ska <- newSrcKeyArena
  empty <- skaNull ska
  assertBool empty

test_arenaReAddAfterRemoveWorks :: IO ()
test_arenaReAddAfterRemoveWorks = do
  ska <- newSrcKeyArena
  skaAppend ska (dr 1) (mkVer 1)
  _ <- skaRemove ska (dr 1)
  skaAppend ska (dr 1) (mkVer 2)
  xs <- skaToList ska
  assertEqual [(dr 1, mkVer 2)] xs

-- | Growth across many entries preserves every previously-written value --
-- the same "growth doesn't corrupt existing data" property 'DefTable.hs'\'s
-- own growth tests check for its columns.
test_arenaGrowthPreservesAllData :: IO ()
test_arenaGrowthPreservesAllData = do
  ska <- newSrcKeyArena
  mapM_ (\i -> skaAppend ska (dr i) (mkVer i)) [1 .. 500 :: Int]
  xs <- skaToList ska
  assertEqual 500 (length xs)
  mapM_ (\i -> assertBool ((dr i, mkVer i) `elem` xs)) [1 .. 500 :: Int]

-- | Many single-entry add/remove cycles against one arena -- the
-- 'SrcKeyArena' analogue of the key-intern churn test above: the live
-- count must track exactly what's currently present, not the number of
-- cycles ever performed.
test_arenaChurnTracksLiveCountOnly :: IO ()
test_arenaChurnTracksLiveCountOnly = do
  ska <- newSrcKeyArena
  forM_ [1 .. 3000 :: Int] $ \n -> do
    skaAppend ska (dr n) (mkVer n)
    _ <- skaRemove ska (dr n)
    pure ()
  n <- skaLiveCount ska
  assertEqual 0 n
  empty <- skaNull ska
  assertBool empty

--
-- Test fixtures: a minimal CompSrc instance (mirrors the "TestStateSrc"
-- pattern used elsewhere in this test suite, e.g. Tests/TestStateIf.hs)
-- purely to construct concrete AnyCompSrcKey/AnyCompSrcVer values.
--

data TestSrc = TestSrc deriving (Show, Eq, Generic, Typeable)

instance Hashable TestSrc

data VoidRequest a

instance CompSrc TestSrc where
  type CompSrcReq TestSrc = VoidRequest
  type CompSrcKey TestSrc = T.Text
  type CompSrcVer TestSrc = Int
  compSrcInstanceId _ = CompSrcInstanceId "TestSrc"
  compSrcExecute _ act = case act of {}
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges _ = retry

testSrcId :: CompSrcId
testSrcId = unTypedCompSrcId (typedCompSrcIdOf TestSrc)

wrapKey :: T.Text -> AnyCompSrcKey
wrapKey t = ForAnyCompFlow testSrcId (Proxy @TestSrc) (SomeCompSrcKey t)

wrapVer :: Int -> AnyCompSrcVer
wrapVer v = ForAnyCompFlow testSrcId (Proxy @TestSrc) (SomeCompSrcVer v)

mkKey :: Int -> AnyCompSrcKey
mkKey n = wrapKey (T.pack (show n))

mkVer :: Int -> AnyCompSrcVer
mkVer = wrapVer

k1, k2 :: AnyCompSrcKey
k1 = mkKey 1
k2 = mkKey 2
