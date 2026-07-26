{-# OPTIONS_GHC -Wno-orphans #-}

{- | The 'ToJSON' instance for @strict-list@'s 'SL.List' below is an orphan
 (neither this module nor "StrictList" defines the class or the type) --
 kept here rather than suppressed globally because this is the only module
 that needs it, via 'MDoc' and friends' generic 'ToJSON' derivation over
 'SL.List'-typed fields.
-}
module Control.Computations.Demos.Hospital.MDoc (
  MDoc (..),
  MSection (..),
  MContent (..),
  MList (..),
  MListItem (..),
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Demos.Utils.FileStore.Types
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Aeson
import qualified Data.Foldable as F
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified StrictList as SL

data MDoc = MDoc
  { m_title :: T.Text
  , m_sections :: SL.List MSection
  }
  deriving (Generic)

instance ToJSON MDoc

data MSection = MSection
  { ms_title :: Option T.Text
  , ms_content :: SL.List MContent
  }
  deriving (Generic)

instance ToJSON MSection

data MContent
  = MContentList MList
  | MContentText T.Text

instance ToJSON MContent where
  toJSON x =
    case x of
      MContentList l -> toJSON l
      MContentText t -> toJSON t

data MList = MList
  {ml_items :: SL.List MListItem}
  deriving (Generic)

instance ToJSON MList where
  toJSON (MList l) = toJSON l

data MListItem = MListItem
  { ml_docId :: Option DocId
  , ml_text :: T.Text
  }
  deriving (Generic)

instance ToJSON MListItem

-- | Orphan instance -- 'SL.List' comes from the @strict-list@ package and
-- has no 'ToJSON' instance of its own. This is the only place in the
-- codebase that needs one: 'MDoc' and friends above use 'SL.List' fields
-- and derive their own 'ToJSON' generically, which recurses into this
-- instance.
instance ToJSON a => ToJSON (SL.List a) where
  toJSON sl = toJSON (F.toList sl)
