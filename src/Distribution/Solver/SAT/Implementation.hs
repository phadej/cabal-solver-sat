module Distribution.Solver.SAT.Implementation (
    satSolver,
) where

import Control.Monad.Trans.State (StateT, execStateT)
import Optics.Core               (at, ix, (%), (.~))
import Optics.State              (use)
import Optics.State.Operators    ((.=), (?=))

import qualified Data.Map.Strict as Map

import Distribution.Solver.SAT.Base
import Distribution.Solver.SAT.DependencyInfo
import Distribution.Solver.SAT.Installed
import Distribution.Solver.SAT.Solver
import Distribution.Solver.SAT.Sources

import Control.Monad.SAT

satSolver :: DependencyResolver
satSolver platform compilerInfo installedIndex sourceIndex _pkgConfigDb _preferences _constraints targets = do
    withFile sourceIndex.location ReadMode $ \sourceIndexHdl -> do
        runSAT $ do
            -- create initial model
            model <- flip execStateT emptyS $ do
                forM_  (installedPackageIndexUnits installedIndex) $ \ip -> do
                    liftIO $ printf "Installed package: %s\n" (prettyShow ip.id)
                    l <- lift newLit
                    v <- lift newLit
                    #model % #packages % at ip.name ?= MkModelPackage
                        { libraries = Map.singleton LMainLibName l
                        , versions  = Map.singleton ip.version $
                            DeepVersion v Map.empty emptyDependencyInfo
                        }

                -- add target packages
                forM_ targets $ \targetPkgName -> do
                    liftIO $ printf "Target package: %s\n" (prettyShow targetPkgName)

                    -- get literals for package components and versions
                    (Identity libLit, verLits) <- getComponentLiterals sourceIndex targetPkgName (Identity LMainLibName)

                    -- require library component to be available.
                    lift $ addClause [libLit]

                    -- require at least one version to be available
                    lift $ assertAtLeastOne (toList verLits)

            modelB <- solve model

            liftIO $ printModel modelB.model

            resultModel <- loop 1 sourceIndexHdl model modelB

            return []
  where
    loop :: Int -> Handle -> S (Lit s) -> S Bool -> SAT s (Model Bool)
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
                        (Identity lnLit, _verLits) <- getComponentLiterals sourceIndex pn (Identity ln)
                        liftIO $ printf "Component %s %s literal %s %s\n" (prettyShow (PackageIdentifier pn ver)) (show ln) (show lnLit) (show verLit)

                        expandCondTree sourceIndex lnLit verLit aflags depends

                    #model % #packages % ix pn % #versions % ix ver .=
                        DeepVersion verLit aflags di

                _ -> return ()

        modelB' <- solve model'

        liftIO $ printBriefModel modelB'.model

        if modelB'.expanded && iteration < 50
        then loop (iteration + 1) sourceIndexHdl model' modelB'
        else return modelB'.model

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

    | DeepVersion a (Map FlagName a) DependencyInfo
      -- ^ the version has been selected, so we expanded it further.
      --
      -- The members are selection literal, automatic flag assignment and dependency map.

    -- TODO: add constructor for installed packages
  deriving (Show, Functor, Foldable, Traversable)

instance HasField "value" (ModelPackageInfo a) a where
    getField (ShallowVersion x)  = x
    getField (DeepVersion x _ _) = x

printModel :: Show a => Model a -> IO ()
printModel m = ifor_ m.packages $ \pn pkg -> do
    printf "package %s\n" (prettyShow pn)
    ifor_ pkg.libraries $ \ln x -> do
        printf "- component %s %s\n" (show ln) (show x)
    ifor_ pkg.versions $ \ver x -> do
        printf "- version %s %s %s\n" (prettyShow ver) (f x) (show x.value)
  where
    f ShallowVersion{} = "shallow"
    f DeepVersion {}   = "expanded"

printBriefModel :: Model Bool -> IO ()
printBriefModel m = ifor_ m.packages $ \pn pkg -> do
    putStrLn $ unwords $
        [ bold (prettyShow pn) ] ++
        [ prettyLibraryName ln | (ln, True) <- Map.toList pkg.libraries ] ++
        [ case x of
            ShallowVersion x'            -> if x' then bold (blue (prettyShow ver)) else blue (prettyShow ver)
            DeepVersion    x' aflags _di -> if x' then bold (prettyShow ver) ++ "(" ++ showFlags aflags ++ ")" else prettyShow ver
        | (ver, x) <- Map.toList pkg.versions
        ]

showFlags :: Map FlagName Bool -> String
showFlags m = unwords [ (if v then '+' else '-') : prettyShow fn | (fn, v) <- Map.toList m ]

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

type MonadSolver s = StateT (S (Lit s)) (SAT s)

assertImplication :: [Lit s] -> [Lit s] -> MonadSolver s ()
assertImplication xs ys = do
    -- liftIO $ putStrLn $ "assertImplication: " ++ show xs ++ show ys
    lift $ addClause $ map neg xs ++ ys

getPackageVersion_ :: SourcePackageIndex -> PackageName -> MonadSolver s (Map Version (Lit s))
getPackageVersion_ sourceIndex pn = do
    let targetVersions = lookupSourcePackage pn sourceIndex

    verLits <- forM targetVersions $ \_ -> lift newLit

    -- TODO: configurable reverse
    lift $ assertAtMostOne $ reverse $ toList verLits

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
    => SourcePackageIndex
    -> PackageName
    -> t LibraryName
    -> MonadSolver s (t (Lit s), Map Version (Lit s))
getComponentLiterals sourceIndex pn lns = do
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
            verLits <- getPackageVersion_ sourceIndex pn
            #model % #packages % at pn ?= MkModelPackage
                { libraries = Map.fromList (toList compLits)
                , versions  = fmap ShallowVersion verLits
                }

            return (fmap snd compLits, verLits)

expandCondTree
    :: forall s. SourcePackageIndex
    -> Lit s                                  -- ^ component literal
    -> Lit s                                  -- ^ version litearal
    -> Map FlagName (Lit s)                   -- ^ automatic flags
    -> CondTree FlagName () DependencyMap     -- ^ dependency info tree
    -> MonadSolver s ()
expandCondTree sourceIndex srcCompLit srcVerLit aflags = go [] where
    go :: [Lit s] -> CondTree FlagName () DependencyMap -> MonadSolver s ()
    go conds (CondNode dm () bs) = do
        forM_ (fromDepMap dm) $ \(Dependency pn vr lns) -> do
            (lnLits, verLits) <- getComponentLiterals sourceIndex pn (toList lns)

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
