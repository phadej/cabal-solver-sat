module Distribution.Solver.SAT.Implementation (
    satSolver,
) where

import Distribution.Solver.SAT.Base
import Distribution.Solver.SAT.DependencyInfo
import Distribution.Solver.SAT.Solver
import Distribution.Solver.SAT.Sources

import Control.Monad.SAT

satSolver :: DependencyResolver
satSolver platform compilerInfo installedIndex sourceIndex _pkgConfigDb _preferences _constraints targets = do
    withFile sourceIndex.location ReadMode $ \sourceIndexHdl -> do
        runSAT $ do
            liftIO $ print targets
            return []

-- | Complete model.
data Model a = MkModel
    { packages :: Map PackageName (ModelPackage a)
    }
  deriving (Show, Functor, Foldable, Traversable)

data ModelPackage a = MkModelPackage
    { libraries :: !(Map LibraryName a)  -- ^ requested libraries.
    , versions  :: !(Map Version (ModelVersion a))
    }
  deriving (Show, Functor, Foldable, Traversable)

data ModelVersion a
    = ShallowVersion a
      -- ^ we have only create a placeholder literal for this version

    | DeepVersion a (Map FlagName a) DependencyInfo
      -- ^ the version has been selected, so we expanded it further.
      --
      -- The members are selection literal, set of available libraries, automatic flag assignment and dependency map.
  deriving (Show, Functor, Foldable, Traversable)
