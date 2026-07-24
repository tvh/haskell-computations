module Control.Computations.CompEngine.CacheBehaviors (
  fullCaching,
  inMemoryShowCaching,
  hashCaching,
) where

----------------------------------------
-- LOCAL
---------------------------------------

import Control.Computations.CompEngine.Types
import Control.Computations.Utils.Hash
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------
import qualified Data.LargeHashable as LH

-- | Stores the value itself in the cache, uses `LargeHashable` to compute the hash.
fullCaching :: LH.LargeHashable a => CompCacheBehavior a
fullCaching = CompCacheBehavior f
 where
  f x = CompCacheValue (Some x) (CompCacheMeta{ccm_largeHash = largeHash128 x})

-- | Only stores the hash in the cache.
hashCaching :: LH.LargeHashable a => CompCacheBehavior a
hashCaching = CompCacheBehavior f
 where
  f x =
    let !h = largeHash128 x
     in CompCacheValue None (CompCacheMeta{ccm_largeHash = h})

-- | Stores the value itself in the cache, uses `show` to compute the hash.
inMemoryShowCaching :: Show a => CompCacheBehavior a
inMemoryShowCaching = CompCacheBehavior f
 where
  f x = CompCacheValue (Some x) (CompCacheMeta{ccm_largeHash = largeHash128 (show x)})
