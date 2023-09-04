module Distribution.Solver.SAT.Implementation (
    satSolver,
) where

import Control.Monad.Trans.State (StateT, execStateT)
import Optics.Core               (at, ix, (%), (.~))
import Optics.State              (use)
import Optics.State.Operators    ((.=), (?=))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Distribution.Solver.SAT.Base
import Distribution.Solver.SAT.Config
import Distribution.Solver.SAT.DependencyInfo
import Distribution.Solver.SAT.Installed
import Distribution.Solver.SAT.Solver
import Distribution.Solver.SAT.Sources

import Control.Monad.SAT

satSolver :: Config -> DependencyResolver
satSolver cfg platform compilerInfo installedIndex sourceIndex _pkgConfigDb _preferences _constraints targets = do
    withFile sourceIndex.location ReadMode $ \sourceIndexHdl -> do
        model <- runSAT $ do
            -- create initial model
            model <- flip execStateT emptyS $ do
                forM_  (installedPackageIndexUnits installedIndex) $ \ip -> do
                    liftIO $ printf "Installed package: %s\n" (prettyShow ip.id)
                    l <- lift newLit
                    v <- lift newLit
                    #model % #packages % at ip.name ?= MkModelPackage
                        { libraries = Map.singleton LMainLibName l
                        , versions  = Map.singleton ip.version $
                            InstalledVersion v ip
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

            liftIO $ printModel modelB.model

            (_, modelC) <- loop 1 sourceIndexHdl model modelB

            liftIO $ print $ modelVersions modelC

            return modelC

        return $ convertModel model
  where
    loop :: Int -> Handle -> S (Lit s) -> S Bool -> SAT s (Model (Lit s), Model Bool)
    loop iteration sourceIndexHdl model modelB = do
        liftIO $ putStrLn "--------------------------------------------------"
        liftIO $ printf "Iteration %d\n" iteration
        liftIO $ putStrLn "--------------------------------------------------"

        model' <- flip execStateT (model & #expanded .~ False) $
            ifor_ modelB.model.packages $ \pn pkg -> when (or pkg.libraries) $
            ifor_ pkg.versions $ \ver def -> case def of
                ShallowVersion True -> do
                    #expanded .= True
                    verLit <- getVersionLiteral pn ver

                    liftIO $ printf "Expanding selected %s (literal %s)\n" (prettyShow (PackageIdentifier pn ver)) (show verLit)
                    gpd <- liftIO $ readSourcePackage sourceIndexHdl pn ver sourceIndex
                    let di = mkDependencyInfo platform compilerInfo gpd

                    aflags <- forM di.autoFlags $ \_ -> lift newLit

                    ifor_ di.libraries $ \ln depends -> do
                        (Identity lnLit, _verLits) <- getComponentLiterals cfg sourceIndex pn (Identity ln)
                        liftIO $ printf "Component %s %s literal %s %s\n" (prettyShow (PackageIdentifier pn ver)) (show ln) (show lnLit) (show verLit)

                        expandCondTree cfg sourceIndex lnLit verLit aflags depends

                    #model % #packages % ix pn % #versions % ix ver .=
                        DeepVersion verLit aflags di

                _ -> return ()

        modelB' <- solve model'

        liftIO $ printModel modelB'.model

        if modelB'.expanded && iteration < cfg.maxIterations
        then loop (iteration + 1) sourceIndexHdl model' modelB'
        else return (model'.model, modelB'.model)

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
showFlags m = unwords [ (if v then '+' else '-') : prettyShow fn | (fn, v) <- Map.toList m ]

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

type MonadSolver s = StateT (S (Lit s)) (SAT s)

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
    mv <- use (#model % #packages % at pn)
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
    mv <- use (#model % #packages % at pn)
    case mv of
        Just pkg -> do
            compLits <- forM lns $ \ln -> case Map.lookup ln pkg.libraries of
                Just l  -> return l
                Nothing -> do
                    l <- lift newLit
                    #model % #packages % ix pn % #libraries % at ln ?= l
                    -- TODO: traverse through existing versions, and if they are expanded and don't have the component set it to false.
                    return l

            return (compLits, fmap (.value) pkg.versions)

        Nothing -> do
            compLits <- traverse (\l -> (,) l <$> lift newLit) lns
            verLits <- getPackageVersion_ cfg sourceIndex pn
            #model % #packages % at pn ?= MkModelPackage
                { libraries = Map.fromList (toList compLits)
                , versions  = fmap ShallowVersion verLits
                }

            return (fmap snd compLits, verLits)

expandCondTree
    :: forall s. Config
    -> SourcePackageIndex
    -> Lit s                                  -- ^ component literal
    -> Lit s                                  -- ^ version litearal
    -> Map FlagName (Lit s)                   -- ^ automatic flags
    -> CondTree FlagName () DependencyMap     -- ^ dependency info tree
    -> MonadSolver s ()
expandCondTree cfg sourceIndex srcCompLit srcVerLit aflags = go [] where
    go :: [Lit s] -> CondTree FlagName () DependencyMap -> MonadSolver s ()
    go conds (CondNode dm () bs) = do
        forM_ (fromDepMap dm) $ \(Dependency pn vr lns) -> do
            (lnLits, verLits) <- getComponentLiterals cfg sourceIndex pn (toList lns)

            -- liftIO $ putStrLn $ prettyShow pn ++ show verLits

            when (null verLits) $ do
                liftIO $ printf "dependency on package without any available versions: %s -> %s\n" "foo" (prettyShow pn)

            let verLits' :: [Lit s]
                verLits' =
                    [ l
                    | (v, l) <- Map.toList verLits
                    , v `withinRange` vr
                    ]

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
