module Distribution.Solver.SAT.Constraints (
    PackageConstraints (..),
) where

-- | For now we don't support Constraints. It's quite hard with SAT.
data PackageConstraints = MkPackageConstraints
