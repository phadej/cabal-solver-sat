module Distribution.Solver.SAT.Implementation (
    satSolver,
) where

import Optics.Core
import Optics.State.Operators
import Control.Monad.Trans.State

import qualified Data.Map.Strict as Map

import Distribution.Solver.SAT.Base
import Distribution.Solver.SAT.DependencyInfo
import Distribution.Solver.SAT.Solver
import Distribution.Solver.SAT.Sources

import Control.Monad.SAT

satSolver :: DependencyResolver
satSolver platform compilerInfo installedIndex sourceIndex _pkgConfigDb _preferences _constraints targets = do
    withFile sourceIndex.location ReadMode $ \sourceIndexHdl -> do
        runSAT $ do

            -- initial model with targets.
            model <- flip execStateT emptyModel $ do 
                forM_ targets $ \targetPkgName -> do
                    liftIO $ printf "Target package: %s\n" (prettyShow targetPkgName)
                    let targetVersions = lookupSourcePackage targetPkgName sourceIndex

                    -- library literal
                    libLit <- lift newLit

                    -- version literals
                    verLits <- forM targetVersions $ \_ -> lift newLit

                    lift $ addClause [libLit]
                    lift $ addClause (toList verLits)

                    #packages % at targetPkgName ?= MkModelPackage
                        { libraries = Map.singleton LMainLibName libLit
                        , versions  = fmap ShallowVersion verLits
                        }

            modelB <- solve model

            liftIO $ print modelB

            return []

-- | Complete model.
data Model a = MkModel
    { packages :: Map PackageName (ModelPackage a)
    }
  deriving (Show, Generic, Functor, Foldable, Traversable)

emptyModel :: Model a
emptyModel = MkModel Map.empty

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
