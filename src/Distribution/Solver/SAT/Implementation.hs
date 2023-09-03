module Distribution.Solver.SAT.Implementation (
    satSolver,
) where

import Control.Monad.Trans.State
import Optics.Core
import Optics.State.Operators
import Optics.State

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

            -- create initial model
            model <- flip execStateT emptyS $ do
                -- add packages in installed index
                -- TODO

                -- add target packages
                forM_ targets $ \targetPkgName -> do
                    liftIO $ printf "Target package: %s\n" (prettyShow targetPkgName)
                    (Identity libLit, _verLits) <- packageVersions sourceIndex targetPkgName (Identity LMainLibName)

                    -- require library component to be available.
                    lift $ addClause [libLit]

            modelB <- solve model

            liftIO $ print modelB

            ifor_ modelB.model.packages $ \pn pkg -> when (or pkg.libraries) $ do
                ifor_ pkg.versions $ \ver def -> case def of
                    ShallowVersion True -> do
                        liftIO $ printf "Selected unexpanded %s\n" (prettyShow (PackageIdentifier pn ver))
                        gpd <- liftIO $ readSourcePackage sourceIndexHdl pn ver sourceIndex
                        let di = mkDependencyInfo platform compilerInfo gpd
                        liftIO $ print di

                    _ -> return ()

            return []

-------------------------------------------------------------------------------
-- Model data definitions
-------------------------------------------------------------------------------

data S a = MkS
    { expanded :: !Bool
    , model    :: !(Model a)
    }
  deriving (Show, Generic, Functor, Foldable, Traversable)

emptyS :: S a
emptyS = MkS
    { expanded = False
    , model    = emptyModel
    }

-- | Complete model.
data Model a = MkModel
    { packages :: Map PackageName (ModelPackage a)
    }
  deriving (Show, Generic, Functor, Foldable, Traversable)

emptyModel :: Model a
emptyModel = MkModel Map.empty

data ModelVersion
    = SourceVersion !Version
  deriving Show

data ModelPackage a = MkModelPackage
    { libraries :: !(Map LibraryName a)  -- ^ requested libraries.
    , versions  :: !(Map Version (ModelPackageInfo a))
    }
  deriving (Show, Functor, Foldable, Traversable)

data ModelPackageInfo a
    = ShallowVersion a
      -- ^ we have only create a placeholder literal for this version

    | DeepVersion a (Map FlagName a) DependencyInfo
      -- ^ the version has been selected, so we expanded it further.
      --
      -- The members are selection literal, automatic flag assignment and dependency map.
  deriving (Show, Functor, Foldable, Traversable)

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

type MonadSolver s = StateT (S (Lit s)) (SAT s)

packageVersions
    :: Traversable t
    => SourcePackageIndex   -- ^ source package inedx
    -> PackageName          -- ^ package name
    -> t LibraryName        -- ^ components
    -> MonadSolver s (t (Lit s), Map Version (Lit s))  -- ^ literals for each component and available versions.
packageVersions sourceIndex pn components = do
    mv <- use (#model % #packages % at pn)
    case mv of
        Just x -> error $ show x --TODO

        Nothing -> do
            let targetVersions = lookupSourcePackage pn sourceIndex

            compLits <- forM components     $ \ln -> (,) ln <$> lift newLit
            verLits  <- forM targetVersions $ \_ -> lift newLit

            lift $ addClause (toList verLits)

            #model % #packages % at pn ?= MkModelPackage
                { libraries = Map.fromList $ toList compLits
                , versions  = fmap ShallowVersion verLits
                }

            return (fmap snd compLits, verLits)
