module Distribution.Solver.SAT.Solver (
    ResolvedPackage (..),
    DependencyResolver,
) where

import Distribution.Solver.SAT.Base
import Distribution.Solver.SAT.Constraints
import Distribution.Solver.SAT.Installed
import Distribution.Solver.SAT.PkgConfig
import Distribution.Solver.SAT.Preferences
import Distribution.Solver.SAT.Sources

import qualified Distribution.Compiler as C
import qualified Distribution.System   as C

-- | Resolved package.
data ResolvedPackage
    = FromSource   PackageIdentifier (Set LibraryName) FlagAssignment
      -- ^ build from source. We only track library components at the moment.
      --
      -- To track exe-dependencies (@build-tool-depends@), tests and benchmarks, we'll need to change to @'Set' 'LibraryName'@ to @'Set' 'ComponentName'@.

    | Preinstalled InstalledPackage
      -- ^ Preinstalled (library) /component/ found in global package db.
  deriving (Show)

-- | Replicating type from @cabal-install-solver@:
--
-- https://hackage.haskell.org/package/cabal-install-solver-3.10.1.0/docs/Distribution-Solver-Types-DependencyResolver.html#t:DependencyResolver
type DependencyResolver =
    C.Platform ->
    C.CompilerInfo ->
    InstalledPackageIndex ->
    SourcePackageIndex ->
    PkgConfigDb ->
    PackagePreferences ->
    PackageConstraints ->
    Set PackageName ->
    IO [ResolvedPackage]
