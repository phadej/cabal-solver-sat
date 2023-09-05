module Distribution.Solver.SAT.Implementation (
    satSolver,
) where

import Control.Exception         (Exception, Handler (..), catches, finally, throwIO)
import Control.Monad.Trans.State (StateT, evalStateT, execStateT)
import Data.IORef                (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Optics.Core               (at, ix, (%), (^?))
import Optics.State              (use)
import Optics.State.Operators    ((.=), (?=))

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import Distribution.Solver.SAT.Base
import Distribution.Solver.SAT.Config
import Distribution.Solver.SAT.DependencyInfo
import Distribution.Solver.SAT.Installed
import Distribution.Solver.SAT.Solver
import Distribution.Solver.SAT.Sources

import Control.Monad.SAT

satSolver :: Config -> DependencyResolver
satSolver cfg platform compilerInfo installedIndex sourceIndex _pkgConfigDb _preferences _constraints targets = do
    stats <- newStats
    solutionRef <- newIORef Nothing

    let handlerIL :: Handler [ResolvedPackage]
        handlerIL = Handler $ \IterationLimit -> do
            s <- readIORef solutionRef
            case s of
                Nothing -> do
                    putStrLn $ magenta "caught UnsatException"
                    throwIO IterationLimit
                Just s' -> return (convertModel s')

        handlerUNSAT :: Handler [ResolvedPackage]
        handlerUNSAT = Handler $ \UnsatException -> do
            s <- readIORef solutionRef
            case s of
                Nothing -> do
                    putStrLn $ magenta "caught UnsatException"
                    epkgs <- readIORef stats.expandedPkgs
                    forM_ (sortOn snd (Map.toList epkgs)) $ \(pn, n) -> when (n > 1) $ printf "%s: %d\n" (prettyShow pn) n
                    throwIO UnsatException
                Just s' -> return (convertModel s')

        printStats :: IO ()
        printStats = when cfg.printStats $ liftIO $ do
            printSection "Statistics"
            printf "Iterations: %d\n" =<< readIORef stats.iteration
            printf "Expansions: %d\n" =<< readIORef stats.expanded

    flip finally printStats $ flip catches [handlerIL, handlerUNSAT] $ withFile sourceIndex.location ReadMode $ \sourceIndexHdl -> do
        model <- runSAT $ do
            -- create initial model
            printSection "Initial model"

            model <- flip execStateT emptyModel $ do
                forM_  (installedPackageIndexUnits installedIndex) $ \ip -> do
                    liftIO $ printf "Installed package: %s\n" (prettyShow ip.id)
                    l <- lift newLit
                    v <- lift newLit
                    #packages % at ip.name ?= MkModelPackage
                        { libraries = Map.singleton LMainLibName l
                        , versions  = Map.singleton ip.version $ InstalledVersion v ip
                        }

                -- add target packages
                forM_ targets $ \targetPkgName -> do
                    liftIO $ printf "Target package: %s\n" (prettyShow targetPkgName)

                    -- get literals for package components and versions
                    (Identity libLit, verLits) <- getComponentLiterals cfg sourceIndex targetPkgName (Identity LMainLibName)

                    -- require library component to be available.
                    lift $ addClause [libLit]

                    -- require at least one version to be available
                    lift $ assertAtLeastOne (toList verLits)

            modelB <- solve model

            when cfg.printModels $ liftIO $ printModel modelB

            (model', modelC) <- loop stats sourceIndexHdl model modelB
            liftIO $ writeIORef solutionRef $ Just modelC

            if cfg.improve > 0
            then do
                (_, modelE) <- improve solutionRef stats sourceIndexHdl model' modelC
                return modelE
            else do
              return modelC

        return $ convertModel model
  where
    improve :: IORef (Maybe (Model Bool)) -> Stats -> Handle -> Model (Lit s) -> Model Bool -> SAT s (Model (Lit s), Model Bool)
    improve solutionRef stats sourceIndexHdl model' modelC = do
        ifor_ (modelVersions modelC) $ \pn ver -> do
            forM_ (model' ^? #packages % ix pn % #versions) $ \vers -> do
                ifor_ vers $ \ver' x -> do
                    when (ver' < ver) $ do
                        addClause [neg x.value]

        lits <- flip evalStateT model' $ forM (Map.toList $ modelVersions modelC) $ \(pn, ver) -> do
            getVersionLiteral pn ver

        addClause $ map neg lits

        modelD <- solve model'
        (model'', modelE) <- loop stats sourceIndexHdl model' modelD

        printSection "Improve differences"
        ifor_ (Map.intersectionWith (,) (modelVersions modelC) (modelVersions modelE)) $ \pn (a, b) -> do
            unless (a == b) $ do
                liftIO $ putStrLn $ unwords [prettyShow pn, prettyShow a, prettyShow b]

        improve solutionRef stats sourceIndexHdl model'' modelE

    loop :: Stats -> Handle -> Model (Lit s) -> Model Bool -> SAT s (Model (Lit s), Model Bool)
    loop stats sourceIndexHdl model modelB = do
        iteration <- liftIO $ readIORef stats.iteration
        expanded  <- liftIO $ readIORef stats.expanded
        liftIO $ modifyIORef' stats.iteration (1 +)

        printSection $ printf "Iteration %d" iteration

        model' <- flip execStateT model $
            ifor_ modelB.packages $ \pn pkg -> when (or pkg.libraries) $
            ifor_ pkg.versions $ \ver def -> case def of
                ShallowVersion True -> do
                    liftIO $ modifyIORef' stats.expanded (1 +)
                    liftIO $ modifyIORef' stats.expandedPkgs $ Map.insertWith (+) pn 1
                    verLit <- getVersionLiteral pn ver

                    liftIO $ printf "Expanding selected %s (literal %s)\n" (prettyShow (PackageIdentifier pn ver)) (show verLit)
                    gpd <- liftIO $ readSourcePackage sourceIndexHdl pn ver sourceIndex
                    let di = mkDependencyInfo platform compilerInfo gpd

                    aflags <- forM di.autoFlags $ \_ -> lift newLit

                    ifor_ di.libraries $ \ln depends -> do
                        (Identity lnLit, _verLits) <- getComponentLiterals cfg sourceIndex pn (Identity ln)
                        liftIO $ printf "Component %s %s literal %s %s\n" (prettyShow (PackageIdentifier pn ver)) (show ln) (show lnLit) (show verLit)

                        expandCondTree cfg sourceIndex (PackageIdentifier pn ver) ln lnLit verLit aflags depends

                    #packages % ix pn % #versions % ix ver .=
                        DeepVersion verLit aflags di

                _ -> return ()

        modelB' <- solve model'

        when cfg.printModels $ do
            printSubsection "Current model"
            liftIO $ printModel modelB'

        expanded' <- liftIO $ readIORef stats.expanded

        if | expanded == expanded'         -> return (model', modelB')
           | iteration < cfg.maxIterations -> loop stats sourceIndexHdl model' modelB'
           | otherwise                     -> liftIO $ throwIO IterationLimit

-------------------------------------------------------------------------------
-- Exceptions
-------------------------------------------------------------------------------

data IterationLimit = IterationLimit deriving Show
instance Exception IterationLimit

-------------------------------------------------------------------------------
-- Statistics
-------------------------------------------------------------------------------

data Stats = MkStats
    { expanded     :: !(IORef Int)
    , iteration    :: !(IORef Int)
    , expandedPkgs :: !(IORef (Map PackageName Int))
    }

newStats :: IO Stats
newStats = do
  expanded <- newIORef 0
  iteration <- newIORef 0
  expandedPkgs <- newIORef Map.empty
  return MkStats {..}

-------------------------------------------------------------------------------
-- Model data definitions
-------------------------------------------------------------------------------

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
  deriving (Show, Generic, Functor, Foldable, Traversable)

data ModelPackageInfo a
    = ShallowVersion a
      -- ^ we have only create a placeholder literal for this version

    | DeepVersion a !(Map FlagName a) !DependencyInfo
      -- ^ the version has been selected, so we expanded it further.
      --
      -- The members are selection literal, automatic flag assignment and dependency map.

    | InstalledVersion a !InstalledPackage
      -- ^ Installed package version
  deriving (Show, Functor, Foldable, Traversable)

instance HasField "value" (ModelPackageInfo a) a where
    getField (ShallowVersion x)     = x
    getField (DeepVersion x _ _)    = x
    getField (InstalledVersion x _) = x

printModel :: Model Bool -> IO ()
printModel m = ifor_ m.packages $ \pn pkg -> do
    putStrLn $ unwords $
        [ bold (prettyShow pn) ] ++
        [ prettyLibraryName ln | (ln, True) <- Map.toList pkg.libraries ] ++
        [ case x of
            ShallowVersion   x'            -> if x' then bold (blue (prettyShow ver)) else blue (prettyShow ver)
            DeepVersion      x' aflags _di -> if x' then bold (prettyShow ver) ++ "(" ++ showFlags aflags ++ ")" else prettyShow ver
            InstalledVersion x' _ip        -> if x' then bold (green (prettyShow ver)) else green (prettyShow ver)
        | (ver, x) <- Map.toList pkg.versions
        ]

showFlags :: Map FlagName Bool -> String
showFlags m = prettyShow $ mkFlagAssignment $ Map.toList m

-------------------------------------------------------------------------------
-- Result construction
-------------------------------------------------------------------------------

-- | Convert model to package name -> version map.
-- This is used for improving the solution.
modelVersions :: Model Bool -> Map PackageName Version
modelVersions m = Map.fromList
    [ (pn, ver)
    | (pn, pkg) <- Map.toList m.packages
    , (ver, x)  <- Map.toList pkg.versions
    , x.value
    ]

-- | Convert model to the final result.
convertModel :: Model Bool -> [ResolvedPackage]
convertModel m = concat
    [ case x of
        InstalledVersion True ip -> [Preinstalled ip]
        DeepVersion True aflags di -> [FromSource (PackageIdentifier pn ver) lns (mkFlagAssignment $ Map.toList $ di.manualFlags <> aflags)]
        _ -> []
    | (pn, pkg) <- Map.toList m.packages
    , let lns = Set.fromList [ ln | (ln, True) <- Map.toList pkg.libraries ]
    , (ver, x)  <- Map.toList pkg.versions
    ]
-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

type MonadSolver s = StateT (Model (Lit s)) (SAT s)

assertImplication :: [Lit s] -> [Lit s] -> MonadSolver s ()
assertImplication xs ys = do
    -- liftIO $ putStrLn $ "assertImplication: " ++ show xs ++ show ys
    lift $ addClause $ map neg xs ++ ys

getPackageVersion_ :: Config -> SourcePackageIndex -> PackageName -> MonadSolver s (Map Version (Lit s))
getPackageVersion_ cfg sourceIndex pn = do
    let targetVersions = lookupSourcePackage pn sourceIndex

    verLits <- forM targetVersions $ \_ -> lift newLit

    let reorder = if cfg.reverse then reverse else id
    lift $ assertAtMostOne $ reorder $ toList verLits

    return verLits

getVersionLiteral :: PackageName -> Version -> MonadSolver s (Lit s)
getVersionLiteral pn ver = do
    mv <- use (#packages % at pn)
    case mv of
        Nothing  -> liftIO $ fail "inconsistency?"
        Just pkg -> case Map.lookup ver pkg.versions of
            Nothing -> liftIO $ fail "inconsistency two?"
            Just pi -> return pi.value

getComponentLiterals
    :: Traversable t
    => Config
    -> SourcePackageIndex
    -> PackageName
    -> t LibraryName
    -> MonadSolver s (t (Lit s), Map Version (Lit s))
getComponentLiterals cfg sourceIndex pn lns = do
    mv <- use (#packages % at pn)
    case mv of
        Just pkg -> do
            compLits <- forM lns $ \ln -> case Map.lookup ln pkg.libraries of
                Just l  -> return l
                Nothing -> do
                    l <- lift newLit
                    #packages % ix pn % #libraries % at ln ?= l
                    -- TODO: traverse through existing versions, and if they are expanded and don't have the component set it to false.
                    return l

            return (compLits, fmap (.value) pkg.versions)

        Nothing -> do
            compLits <- traverse (\l -> (,) l <$> lift newLit) lns
            verLits <- getPackageVersion_ cfg sourceIndex pn
            #packages % at pn ?= MkModelPackage
                { libraries = Map.fromList (toList compLits)
                , versions  = fmap ShallowVersion verLits
                }

            return (fmap snd compLits, verLits)

expandCondTree
    :: forall s. Config
    -> SourcePackageIndex
    -> PackageIdentifier                      -- ^ package identifier
    -> LibraryName                            -- ^ component name
    -> Lit s                                  -- ^ component literal
    -> Lit s                                  -- ^ version litearal
    -> Map FlagName (Lit s)                   -- ^ automatic flags
    -> CondTree FlagName () DependencyMap     -- ^ dependency info tree
    -> MonadSolver s ()
expandCondTree cfg sourceIndex pi ln srcCompLit srcVerLit aflags = go [] where
    go :: [Lit s] -> CondTree FlagName () DependencyMap -> MonadSolver s ()
    go conds (CondNode dm () bs) = do
        forM_ (fromDepMap dm) $ \(Dependency pn vr lns) -> do
            (lnLits, verLits) <- getComponentLiterals cfg sourceIndex pn (toList lns)

            let verLits' :: [Lit s]
                verLits' =
                    [ l
                    | (v, l) <- Map.toList verLits
                    , v `withinRange` vr
                    ]

            -- if there are no versions matching the range, print a warning
            when (null verLits') $ do
                liftIO $ putStrLn $ magenta $ printf "dependency on package without any available versions: %s %s -> %s %s" (prettyShow pi) (prettyLibraryName ln) (prettyShow pn) (prettyShow vr)

            -- component depends on library components..
            forM_ lnLits $ \libLit -> do
                assertImplication (srcCompLit : srcVerLit : conds) [libLit]

            -- component depends on package versions..
            -- at least one has to be selected.
            assertImplication (srcCompLit : srcVerLit : conds) verLits'

        forM_ bs $ \(CondBranch c t mf) -> do
            c' <- lift $ addDefinition (conditionToProp c)
            go (c':conds) t
            forM_ mf $ go (neg c':conds)

    conditionToProp :: Condition FlagName -> Prop s
    conditionToProp (Var f) = case Map.lookup f aflags of
        Nothing -> false
        Just l  -> lit l
    conditionToProp (Lit True) = true
    conditionToProp (Lit False) = false
    conditionToProp (CNot c)    = neg (conditionToProp c)
    conditionToProp (COr x y)   = conditionToProp x \/ conditionToProp y
    conditionToProp (CAnd x y)  = conditionToProp x /\ conditionToProp y
