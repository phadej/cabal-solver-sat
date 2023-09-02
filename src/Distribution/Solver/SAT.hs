module Distribution.Solver.SAT (
    DependencyResolver,
    module X,
) where

import Distribution.Solver.SAT.Base

import Distribution.Solver.SAT.Constraints as X
import Distribution.Solver.SAT.Installed   as X
import Distribution.Solver.SAT.PkgConfig   as X
import Distribution.Solver.SAT.Preferences as X
import Distribution.Solver.SAT.Solver      as X
import Distribution.Solver.SAT.Sources     as X

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
