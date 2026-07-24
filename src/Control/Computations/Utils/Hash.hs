{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Computations.Utils.Hash (
  Hash128 (..),
  largeHash128,
  hashToHexText,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

----------------------------------------
-- EXTERNAL
----------------------------------------
import qualified Data.ByteString.Base16 as Base16
import Data.Hashable
import qualified Data.LargeHashable as LH
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import GHC.Generics (Generic)

newtype Hash128 = Hash128 {unHash128 :: LH.Word128}
  deriving stock (Typeable, Generic)
  deriving newtype (Eq, Ord, Hashable)

-- Hand-rolled rather than Generic-derived: cap_hash (Hash128) is compared
-- and hashed on essentially every cache lookup/CompAp dispatch, and
-- profiling showed the Generic-derived default walking Word128's
-- representation was a measurable cost. Word128 is just two Word64s, so
-- hash them directly -- equal values still hash equal.
instance Hashable LH.Word128 where
  hashWithSalt s w =
    s `hashWithSalt` LH.w128_first w `hashWithSalt` LH.w128_second w
instance LH.LargeHashable LH.Word128
instance LH.LargeHashable Hash128

largeHash128 :: LH.LargeHashable a => a -> Hash128
largeHash128 x =
  let h = LH.largeHash LH.md5HashAlgorithm x
   in Hash128 (LH.unMD5Hash h)

hashToHexText :: Hash128 -> T.Text
hashToHexText (Hash128 w) = T.decodeUtf8 (Base16.encode (LH.w128ToBs w))

instance Show Hash128 where
  showsPrec _ h = showString (T.unpack (hashToHexText h))
