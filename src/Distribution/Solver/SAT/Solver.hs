module Distribution.Solver.SAT.Solver (
    ResolvedPackage (..),
) where

import Distribution.Solver.SAT.Base
import Distribution.Solver.SAT.Installed

-- | Resolved package.
data ResolvedPackage
    = FromSource   PackageIdentifier (Set LibraryName) FlagAssignment
      -- ^ build from source. We are track libraries only atm.
      -- 
      -- To track exe-dependencies (@build-tool-depends@), tests and benchmarks, we'll need to change to @'Set' 'LibraryName'@ to @'Set' 'ComponentName'@.

    | Preinstalled InstalledPackage
      -- ^ Preinstalled (library) /component/ found in global package db.
  deriving (Show)
