module Distribution.Solver.SAT where

import Distribution.Solver.SAT.Base
import Distribution.Solver.SAT.Constraints
import Distribution.Solver.SAT.Installed
import Distribution.Solver.SAT.PkgConfig
import Distribution.Solver.SAT.Preferences
import Distribution.Solver.SAT.Sources

import qualified Distribution.Compiler as C
import qualified Distribution.System   as C

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

-- | Source package index, i.e. all packages to be built.
-- Includes the local packages as well (which shadow repositories).
type SourcePackageIndex = Map PackageName (Map Version TarEntryOffset)

-- | Resolved package.
data ResolvedPackage
    = FromSource   PackageIdentifier FlagAssignment
    | Preinstalled InstalledPackage
  deriving (Show)
