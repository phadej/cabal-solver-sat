module Distribution.Solver.SAT.PkgConfig (
    PkgConfigDb (..),
) where

-- | For now we don't support @pkg-config@ dependencies.
-- We assume they are always satisfied.
data PkgConfigDb = MkPkgConfigDb
