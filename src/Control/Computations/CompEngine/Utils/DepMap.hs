{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Just the 'IsDep' typeclass now -- the module used to also define a
 'DepMap' container (a forward+reverse dependency index, its reverse side
 bucketed by dependency version via "Control.Computations.CompEngine.Utils.VerList")
 that "Control.Computations.CompEngine.SimpleStateIf" used as one of Stage
 0/1's five persistent containers. The columnar rewrite
 (docs/benchmark-notes.md "Memory roadmap") replaced it with per-def
 'Control.Computations.CompEngine.Utils.DefTable' columns (flat rdeps plus
 a changed bit, dropping the version-bucketing 'VerList' level entirely --
 see 'SimpleStateIf's module haddock for why per-edge /observed/ version
 tracking is still needed and where it now lives). Both the container and
 'VerList.hs' are deleted as dead code; 'IsDep'/'IsDepConstraints' stay --
 they're load-bearing for "Control.Computations.CompEngine.Types"'s
 'CompEngDep'/'CompDep' instances and
 "Control.Computations.CompEngine.CompSrc"'s 'AnyCompSrcDep' instance,
 unrelated to the now-dead container.
-}
module Control.Computations.CompEngine.Utils.DepMap (
  IsDep (..),
  IsDepConstraints,
)
where

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Hashable

type IsDepConstraints a = (Show a, Eq a, Hashable a)

class
  ( IsDepConstraints a
  , IsDepConstraints (DepKey a)
  , IsDepConstraints (DepVer a)
  ) =>
  IsDep a
  where
  type DepKey a
  type DepVer a
  depKey :: a -> DepKey a
  depVer :: a -> DepVer a
