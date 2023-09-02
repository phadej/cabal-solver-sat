module Distribution.Solver.SAT.Implementation (
    satSolver,
) where

import Distribution.Solver.SAT.Base
import Distribution.Solver.SAT.Solver
import Distribution.Solver.SAT.Sources

import Control.Monad.SAT

satSolver :: DependencyResolver
satSolver platform compilerInfo installedIndex sourceIndex _pkgConfigDb _preferences _constraints targets = do
    withFile sourceIndex.location ReadMode $ \sourceIndexHdl -> do
        runSAT $ do
            liftIO $ print targets
            return []

data Model a
  deriving (Functor, Foldable, Traversable)
