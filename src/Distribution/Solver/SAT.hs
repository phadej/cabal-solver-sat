module Distribution.Solver.SAT where

import  Distribution.Solver.SAT.Base
import  Distribution.Solver.SAT.Installed

import qualified Distribution.System as C
import qualified Distribution.Compiler as C

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
    [LabeledPackageConstraint] ->
    Set PackageName ->
    IO [ResolvedPackage]


-- | Source package index, i.e. all packages to be built.
-- Includes the local packages as well (which shadow repositories).
type SourcePackageIndex = ()

-- | For now we don't support @pkg-config@ dependencies.
-- We assume they are always satisfied.
type PkgConfigDb = ()

-- | For now we don't support preferences. It's quite hard with SAT.
type PackagePreferences = ()

-- | For now there aren't external constraints.
type LabeledPackageConstraint = Void

-- | Resolved package.
data ResolvedPackage
    = FromSource   SourcePackage
    | Preinstalled InstalledPackage
  deriving (Show)

type SourcePackage    = () -- PackageId + flag assignment.
