module Distribution.Solver.SAT.Implementation (
    satSolver,
) where

import Control.Exception            (Exception, Handler (..), catches, finally, throwIO)
import Control.Monad.Trans.State    (StateT, evalStateT, execStateT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.IORef                   (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Distribution.Solver.SAT.DMap (DMap, DSum (..))
import Optics.Core                  (at, ix, (%), (^?))
import Optics.State                 (use)
import Optics.State.Operators       ((%=), (?=))

import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set
import qualified Distribution.Solver.SAT.DMap as DMap

import Distribution.Solver.SAT.Base
import Distribution.Solver.SAT.Config
import Distribution.Solver.SAT.DependencyInfo
import Distribution.Solver.SAT.Installed
import Distribution.Solver.SAT.Solver
import Distribution.Solver.SAT.Sources

import Control.Monad.SAT

satSolver :: forall loc. Config -> DependencyResolver loc
satSolver cfg platform compilerInfo installedIndex sourceIndex _pkgConfigDb _preferences _constraints targets =
  openSourcePackageIndex sourceIndex $ \sourceIndex' -> do
    stats <- newStats
    let handlerUNSAT :: Handler [ResolvedPackage loc]
        handlerUNSAT = Handler $ \UnsatException -> do
            putStrLn $ magenta "caught UnsatException"
            epkgs <- readIORef stats.expandedPkgs
            forM_ (sortOn snd (Map.toList epkgs)) $ \(pn, n) -> when (n > 1) $ printf "%s: %d\n" (prettyShow pn) n
            throwIO UnsatException

    let printStats :: IO ()
        printStats = when cfg.printStats $ liftIO $ do
            printSection "Statistics"
            printf "Iterations:    %d\n" =<< readIORef stats.iteration
            printf "Improvements:  %d\n" =<< readIORef stats.improvements
            printf "Expansions:    %d\n" =<< readIORef stats.expanded
            printf "SAT variables: %d\n" =<< readIORef stats.literals
            printf "SAT clauses:   %d\n" =<< readIORef stats.clauses

    flip finally printStats $ flip catches [handlerUNSAT] $ do

        model <- runSAT $ do
            -- create initial model
            printSection "Initial model"

            model <- flip execStateT emptyModel $ do
                forM_  (installedPackageIndexUnits installedIndex) $ \ip -> do
                    liftIO $ printf "Installed package: %s\n" (prettyShow ip.id)

                    let targetVersions = lookupSourcePackage ip.name sourceIndex'

                    -- TODO
                    -- unless (null targetVersions) $ liftIO $ fail $ "installed package has versions in source index: " ++ show ip.name

                    l <- lift newLit
                    v <- lift newLit
                    #packages % at ip.name ?= MkModelPackage
                        { components = Map.singleton (CLibName LMainLibName) l
                        , versions   = DMap.singleton (InstalledVersion ip.version ip.unitId) (InstalledInfo v ip)
                        }

                -- add target packages
                forM_ targets $ \(targetPkgName, cn) -> do
                    liftIO $ printf "Target component: %s %s\n" (prettyShow targetPkgName) (prettyShow cn)

                    -- get literals for package components and versions
                    (Identity compLit, verLits) <- getComponentLiterals cfg sourceIndex' targetPkgName (Identity cn)

                    -- require library component to be available.
                    lift $ addClause [compLit]

                    -- require at least one version to be available
                    lift $ assertAtLeastOne (toList verLits)

            saveSATstats stats
            modelB <- solve model
            modelRef <- liftIO $ newIORef model

            when cfg.printModels $ liftIO $ printModel modelB

            modelC <- loop stats sourceIndex' modelRef modelB
            modelE <- improve stats sourceIndex' modelRef modelC
            return modelE

        return $ convertModel model
  where
    saveSATstats :: Stats -> SAT s ()
    saveSATstats stats = do
        clauseN <- numberOfClauses
        variableN <- numberOfVariables
        liftIO $ do
            modifyIORef' stats.clauses $ max clauseN
            writeIORef stats.literals variableN

    expand
        :: Stats
        -> OpenedSourcePackageIndex srcpkg loc
        -> IORef (Model srcpkg loc (Lit s))
        -> Model srcpkg loc Bool
        -> SAT s (Model srcpkg loc (Lit s))
    expand stats sourceIndex' modelRef modelB = do
        model  <- liftIO $ readIORef modelRef
        model' <- flip execStateT model $
            ifor_ modelB.packages $ \pn pkg -> when (or pkg.components) $
            for_ (DMap.toList pkg.versions) $ \(ver :&: def) -> case def of
                ShallowInfo True i -> do
                    liftIO $ modifyIORef' stats.expanded (1 +)
                    liftIO $ modifyIORef' stats.expandedPkgs $ Map.insertWith (+) pn 1
                    verLit <- getVersionLiteral pn ver

                    liftIO $ printf "Package %s (literal %s)\n" (prettyShow (PackageIdentifier pn ver.version)) (show verLit)
                    gpd <- liftIO $ readSourcePackage i sourceIndex'
                    let di = mkDependencyInfo platform compilerInfo gpd.description

                    aflags <- forM di.autoFlags $ \_ -> lift newLit

                    ifor_ di.components $ \cn depends -> do
                        (Identity cnLit, _verLits) <- getComponentLiterals cfg sourceIndex' pn (Identity cn)
                        liftIO $ printf "     -> %s %s (literals %s %s)\n" (prettyShow (PackageIdentifier pn ver.version)) (prettyShow cn) (show verLit) (show cnLit)

                        expandCondTree cfg sourceIndex' (PackageIdentifier pn ver.version) cn cnLit verLit aflags depends

                    #packages % ix pn % #versions %=
                            DMap.insert ver (DeepInfo verLit gpd.location aflags di)

                _ -> return ()

        liftIO $ writeIORef modelRef model'
        return model'

    loop
        :: Stats
        -> OpenedSourcePackageIndex srcpkg loc
        -> IORef (Model srcpkg loc (Lit s))
        -> Model srcpkg loc Bool
        -> SAT s (Model srcpkg loc Bool)
    loop stats sourceIndex' modelRef modelB =
        runIdentityT (loopTrans (\m -> IdentityT (solve m)) stats sourceIndex' modelRef modelB)

    loopAssuming
        :: Lit s
        -> Stats
        -> OpenedSourcePackageIndex srcpkg loc
        -> IORef (Model srcpkg loc (Lit s))
        -> Model srcpkg loc Bool
        -> SAT s (Maybe (Model srcpkg loc Bool))
    loopAssuming ass stats sourceIndex' modelRef modelB = 
        runMaybeT (loopTrans (\m -> MaybeT (solveAssuming m (pure ass))) stats sourceIndex' modelRef modelB)

    loopTrans
        :: (MonadTrans t, Monad (t (SAT s)))
        => (Model srcpkg loc (Lit s) -> t (SAT s) (Model srcpkg loc Bool))
        -> Stats
        -> OpenedSourcePackageIndex srcpkg loc
        -> IORef (Model srcpkg loc (Lit s))
        -> Model srcpkg loc Bool
        -> t (SAT s) (Model srcpkg loc Bool)
    loopTrans solveFun stats sourceIndex' modelRef modelB = do
        iteration <- lift $ liftIO $ readIORef stats.iteration
        expanded  <- lift $ liftIO $ readIORef stats.expanded

        lift $ liftIO $ modifyIORef' stats.iteration (1 +)
        lift $ printSection $ printf "Iteration %d" iteration
        model' <- lift $ expand stats sourceIndex' modelRef modelB

        expanded' <- lift $ liftIO $ readIORef stats.expanded
        if | expanded == expanded'          -> return modelB
           | iteration >= cfg.maxIterations -> lift $ liftIO $ throwIO IterationLimit
           | otherwise -> do
              lift $ saveSATstats stats
              modelB' <- solveFun model'
              lift $ when cfg.printModels $ do
                  printSubsection "Current model"
                  liftIO $ printModel modelB'

              loopTrans solveFun stats sourceIndex' modelRef modelB'


    improve
        :: Stats
        -> OpenedSourcePackageIndex srcpkg loc
        -> IORef (Model srcpkg loc (Lit s))
        -> Model srcpkg loc Bool
        -> SAT s (Model srcpkg loc Bool)
    improve stats sourceIndexHdl modelRef modelC = do
        improvements <- liftIO $ readIORef stats.improvements
        if improvements >= cfg.improve
        then return modelC
        else do
            model' <- liftIO $ readIORef modelRef

            -- forbid older versions
            ifor_ (modelVersions modelC) $ \pn (Some ver) -> do
                forM_ (model' ^? #packages % ix pn % #versions) $ \vers -> do
                    for_ (DMap.toList vers) $ \(ver' :&: x) -> do
                        when (ver'.version < ver.version) $ do
                            addClause [neg x.value]

            lits <- flip evalStateT model' $ forM (Map.toList $ modelVersions modelC) $ \(pn, Some ver) -> do
                getVersionLiteral pn ver

            andDef <- newLit
            addConjDefinition andDef lits
            ok <- solveAssuming model' (pure (neg andDef))

            case ok of
                Nothing -> return modelC
                Just modelD -> do
                    saveSATstats stats
                    mmodelE <- loopAssuming (neg andDef) stats sourceIndexHdl modelRef modelD
                    case mmodelE of
                        Nothing -> return modelC
                        Just modelE -> do
                            addClause [neg andDef]

                            printSection "Improve differences"
                            ifor_ (Map.intersectionWith (,) (modelVersions modelC) (modelVersions modelE)) $ \pn (Some a, Some b) -> do
                                unless (eqp a b) $ do
                                    liftIO $ putStrLn $ unwords [prettyShow pn, prettyShow a.version, prettyShow b.version]

                            liftIO $ modifyIORef' stats.improvements (1 +)

                            improve stats sourceIndexHdl modelRef modelE

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
    , improvements :: !(IORef Int)
    , expandedPkgs :: !(IORef (Map PackageName Int))
    , literals     :: !(IORef Int)
    , clauses      :: !(IORef Int)
    }

newStats :: IO Stats
newStats = do
  expanded <- newIORef 0
  iteration <- newIORef 0
  improvements <- newIORef 0
  expandedPkgs <- newIORef Map.empty
  literals <- newIORef 0
  clauses  <- newIORef 0
  return MkStats {..}

-------------------------------------------------------------------------------
-- Model data definitions
-------------------------------------------------------------------------------

-- | Complete model.
data Model srcpkg loc a = MkModel
    { packages :: Map PackageName (ModelPackage srcpkg loc a)
    }
  deriving (Generic, Functor, Foldable, Traversable)

emptyModel :: Model srcpkg loc a
emptyModel = MkModel Map.empty

data ModelKind = Source | Installed

type ModelVersion :: ModelKind -> Type
data ModelVersion k where
    SourceVersion    :: !Version -> ModelVersion Source
    InstalledVersion :: !Version -> !UnitId -> ModelVersion Installed

instance Eq (ModelVersion k) where
    (==) (SourceVersion x)      (SourceVersion u)      = x == u
    (==) (InstalledVersion x y) (InstalledVersion u v) = x == u && y == v

instance EqP ModelVersion where
    eqp (SourceVersion x)      (SourceVersion u)      = x == u
    eqp (InstalledVersion x y) (InstalledVersion u v) = x == u && y == v
    eqp _                      _                      = False

instance GEq ModelVersion where
    geq (SourceVersion x)      (SourceVersion u)
        | x == u = Just Refl
    geq (InstalledVersion x y) (InstalledVersion u v)
        | x == u, y == v = Just Refl
    geq _ _ = Nothing

instance GCompare ModelVersion where
    gcompare (SourceVersion x)      (SourceVersion y) =
        case compare x y of
            GT -> GGT
            EQ -> GEQ
            LT -> GLT
    gcompare (SourceVersion _)      (InstalledVersion _ _) = GLT
    gcompare (InstalledVersion _ _) (SourceVersion _)      = GGT
    gcompare (InstalledVersion x u) (InstalledVersion y v) =
        case compare x y <> compare u v of
            GT -> GGT
            EQ -> GEQ
            LT -> GLT

deriving instance Show (ModelVersion k)

instance HasField "version" (ModelVersion k) Version where
    getField (SourceVersion v)      = v
    getField (InstalledVersion v _) = v

data ModelPackage srcpkg loc a = MkModelPackage
    { components :: !(Map ComponentName a)               -- ^ (enabled) components
    , versions   :: !(DMap ModelVersion (ModelPackageInfo srcpkg loc) a)
    }
  deriving (Generic, Functor, Foldable, Traversable)

type ModelPackageInfo :: Type -> Type -> ModelKind -> Type -> Type
data ModelPackageInfo srcpkg loc k a where
    -- | we have only create a placeholder literal for this version
    ShallowInfo :: a -> !srcpkg -> ModelPackageInfo srcpkg loc Source a

    -- | the version has been selected, so we expanded it further.
    --
    -- The members are selection literal, automatic flag assignment and dependency map.
    DeepInfo :: a -> !loc -> !(Map FlagName a) -> !DependencyInfo -> ModelPackageInfo srcpkg loc Source a

    -- | Installed package version
    InstalledInfo :: a -> !InstalledPackage -> ModelPackageInfo srcpkg loc Installed a

deriving instance Functor (ModelPackageInfo srcpkg loc k)
deriving instance Foldable (ModelPackageInfo srcpkg loc k)
deriving instance Traversable (ModelPackageInfo srcpkg loc k)

instance HasField "value" (ModelPackageInfo srcpkg loc k a) a where
    getField (ShallowInfo x _)   = x
    getField (DeepInfo x _ _ _)  = x
    getField (InstalledInfo x _) = x

printModel :: Model srcpkg loc Bool -> IO ()
printModel m = ifor_ m.packages $ \pn pkg -> do
    putStrLn $ unwords $
        [ bold (prettyShow pn) ] ++
        [ prettyShow cn | (cn, True) <- Map.toList pkg.components ] ++
        [ case x of
            ShallowInfo   x' _            -> if x' then bold (blue (prettyShow ver.version)) else blue (prettyShow ver.version)
            DeepInfo      x' _ aflags _di -> if x' then bold (prettyShow ver.version) ++ "(" ++ showFlags aflags ++ ")" else prettyShow ver.version
            InstalledInfo x' _ip          -> if x' then bold (green (prettyShow ver.version)) else green (prettyShow ver.version)
        | (ver :&: x) <- DMap.toList pkg.versions
        ]

showFlags :: Map FlagName Bool -> String
showFlags m = prettyShow $ mkFlagAssignment $ Map.toList m

-------------------------------------------------------------------------------
-- Result construction
-------------------------------------------------------------------------------

-- | Convert model to package name -> version map.
-- This is used for improving the solution.
modelVersions :: Model srcpkg loc Bool -> Map PackageName (Some ModelVersion)
modelVersions m = Map.fromList
    [ (pn, Some ver)
    | (pn, pkg) <- Map.toList m.packages
    , (ver :&: x) <- DMap.toList pkg.versions
    , x.value
    ]

-- | Convert model to the final result.
convertModel :: Model srcpkg loc Bool -> [ResolvedPackage loc]
convertModel m = concat
    [ case x of
        InstalledInfo True ip       -> [Preinstalled ip]
        DeepInfo True loc aflags di -> [FromSource loc (PackageIdentifier pn ver.version) lns (mkFlagAssignment $ Map.toList $ di.manualFlags <> aflags)]
        _ -> []
    | (pn, pkg) <- Map.toList m.packages
    , let lns = Set.fromList [ ln | (ln, True) <- Map.toList pkg.components ]
    , (ver :&: x)  <- DMap.toList pkg.versions
    ]

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

type MonadSolver srcpkg loc s = StateT (Model srcpkg loc (Lit s)) (SAT s)

assertImplication :: [Lit s] -> [Lit s] -> MonadSolver srcpkg loc s ()
assertImplication xs ys = do
    -- liftIO $ putStrLn $ "assertImplication: " ++ show xs ++ show ys
    lift $ addClause $ map neg xs ++ ys

getVersionLiteral :: PackageName -> ModelVersion k -> MonadSolver srcpkg loc s (Lit s)
getVersionLiteral pn ver = do
    mv <- use (#packages % at pn)
    case mv of
        Nothing  -> liftIO $ fail "inconsistency?"
        Just pkg -> case DMap.lookup ver pkg.versions of
            Nothing -> liftIO $ fail "inconsistency two?"
            Just pi -> return pi.value

getComponentLiterals
    :: forall t srcpkg loc s. Traversable t
    => Config
    -> OpenedSourcePackageIndex srcpkg loc
    -> PackageName
    -> t ComponentName
    -> MonadSolver srcpkg loc s (t (Lit s), Map (Some ModelVersion) (Lit s))
getComponentLiterals cfg sourceIndex pn lns = do
    mv <- use (#packages % at pn)
    case mv of
        Just pkg -> do
            compLits <- forM lns $ \ln -> case Map.lookup ln pkg.components of
                Just l  -> return l
                Nothing -> do
                    l <- lift newLit
                    #packages % ix pn % #components % at ln ?= l
                    -- TODO: traverse through existing versions, and if they are expanded and don't have the component set it to false.
                    return l

            return (compLits, Map.fromList [ (Some ver, x.value) | ver :&: x <- DMap.toList pkg.versions ])

        Nothing -> do
            compLits <- traverse (\l -> (,) l <$> lift newLit) lns
            verLits <- getPackageVersion_
            let verLits'  = Map.mapKeys (\v -> Some (SourceVersion v)) verLits
                verLits'' = DMap.fromList [ SourceVersion v :&: ShallowInfo l i | (v, (i, l)) <- Map.toList verLits ]
            #packages % at pn ?= MkModelPackage
                { components = Map.fromList (toList compLits)
                , versions   = verLits''
                }

            return (fmap snd compLits, fmap snd verLits')
  where
    getPackageVersion_ :: MonadSolver srcpkg loc s (Map Version (srcpkg, Lit s))
    getPackageVersion_ = do
        let targetVersions = lookupSourcePackage pn sourceIndex

        verLits <- forM targetVersions $ \i -> (,) i <$> lift newLit

        let reorder = if cfg.reverse then reverse else id
        lift $ assertAtMostOne $ reorder $ map snd $ toList verLits

        return verLits


expandCondTree
    :: forall s srcpkg loc. Config
    -> OpenedSourcePackageIndex srcpkg loc
    -> PackageIdentifier                      -- ^ package identifier
    -> ComponentName                          -- ^ component name
    -> Lit s                                  -- ^ component literal
    -> Lit s                                  -- ^ version litearal
    -> Map FlagName (Lit s)                   -- ^ automatic flags
    -> CondTree FlagName () DependencyMap     -- ^ dependency info tree
    -> MonadSolver srcpkg loc s ()
expandCondTree cfg sourceIndex pi ln srcCompLit srcVerLit aflags = go [] where
    go :: [Lit s] -> CondTree FlagName () DependencyMap -> MonadSolver srcpkg loc s ()
    go conds (CondNode dm () bs) = do
        forM_ (fromDepMap dm) $ \(Dependency pn vr lns) -> do
            (lnLits, verLits) <- getComponentLiterals cfg sourceIndex pn (map CLibName (toList lns))

            let verLits' :: [Lit s]
                verLits' =
                    [ l
                    | (Some v, l) <- Map.toList verLits
                    , v.version `withinRange` vr
                    ]

            -- if there are no versions matching the range, print a warning
            when (null verLits') $ do
                liftIO $ putStrLn $ magenta $ printf "dependency on package without any available versions: %s %s -> %s %s" (prettyShow pi) (prettyShow ln) (prettyShow pn) (prettyShow vr)

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
