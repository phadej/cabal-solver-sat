module Distribution.Solver.SAT.Sources (
    SourcePackageIndex (..),
    SourcePackage (..),
) where

import Distribution.Solver.SAT.Base

-- | Source package index, i.e. all packages to be built.
-- Includes the local packages as well (which shadow repositories).
data SourcePackageIndex = MkSourcePackageIndex (Map PackageName (Map Version SourcePackage))
  deriving Show

data SourcePackage
    = ProjectPackage !FilePath       -- ^ local, project package.
    | RemotePackage !TarEntryOffset  -- ^ package in a source-repository. tar entry offset tells which.
  deriving Show
