module Distribution.Solver.SAT.Preferences (
    PackagePreferences (..),
) where

-- | For now we don't support preferences. It's quite hard with SAT.
data PackagePreferences = MkPackagePreferences
