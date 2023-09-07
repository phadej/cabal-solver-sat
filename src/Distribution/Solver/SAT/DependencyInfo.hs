module Distribution.Solver.SAT.DependencyInfo (
    DependencyInfo (..),
    mkDependencyInfo,
) where

import Distribution.Solver.SAT.Base

import qualified Distribution.Compat.Lens          as L
import qualified Distribution.Compiler             as C
import qualified Distribution.PackageDescription   as C
import qualified Distribution.System               as C
import qualified Distribution.Types.BuildInfo.Lens as L

import qualified Data.Map as Map

data DependencyInfo = MkDependencyInfo
    { manualFlags :: !(Map FlagName Bool)
    , autoFlags   :: !(Map FlagName Bool)
    , components  :: !(Map ComponentName (CondTree FlagName () DependencyMap))
    }
  deriving Show

mkDependencyInfo :: C.Platform -> C.CompilerInfo -> GenericPackageDescription -> DependencyInfo
mkDependencyInfo platform compilerInfo gpd = MkDependencyInfo
    { manualFlags = mflags
    , autoFlags   = aflags
    , components  = Map.fromList $ mainLib ++ subLibs ++ exes
    }
  where
    mainLib = case condLibrary gpd of
        Nothing -> []
        Just l  -> [(CLibName C.LMainLibName, extract l)]

    subLibs =
        [ (CLibName $ C.LSubLibName ln, extract l)
        | (ln, l) <- condSubLibraries gpd
        ]

    exes =
        [ (CExeName cn, extract l)
        | (cn, l) <- condExecutables gpd
        ]

    -- TODO: tests, benchmarks and flibs

    mflags :: Map FlagName Bool
    mflags = manualFlags gpd

    aflags :: Map FlagName Bool
    aflags = automaticFlags gpd

    extract :: L.HasBuildInfo a => CondTree C.ConfVar [C.Dependency] a -> CondTree FlagName () DependencyMap
    extract l = fmap (toDepMap . C.targetBuildDepends) $ extract2 platform compilerInfo mflags (extract1 l)

-------------------------------------------------------------------------------
-- Extracting dependency info.
-------------------------------------------------------------------------------

extract1 :: L.HasBuildInfo a => CondTree C.ConfVar [C.Dependency] a -> CondTree C.ConfVar () C.BuildInfo
extract1 = C.mapCondTree (L.view L.buildInfo) (const ()) id

extract2 :: (Semigroup c, Semigroup a) => C.Platform -> C.CompilerInfo -> Map FlagName Bool -> CondTree C.ConfVar c a -> CondTree FlagName c a
extract2 (C.Platform arch os) compilerInfo mflags = simplifyCondTree' (simplifyConfVar os arch compilerInfo mflags)

manualFlags :: GenericPackageDescription -> Map FlagName Bool
manualFlags gpd = Map.fromList
    [ (fn, v)
    | C.MkPackageFlag { flagName = fn, flagDefault = v, flagManual = True } <- genPackageFlags gpd
    ]

automaticFlags :: GenericPackageDescription -> Map FlagName Bool
automaticFlags gpd = Map.fromList
    [ (fn, v)
    | C.MkPackageFlag { flagName = fn, flagDefault = v, flagManual = False } <- genPackageFlags gpd
    ]

simplifyCondTree'
    :: forall a d u v. (Semigroup a, Semigroup d)
    => (v -> Either u Bool)
    -> CondTree v d a
    -> CondTree u d a
simplifyCondTree' env (CondNode a d ifs) = do
    let (ad1, ifs1) = partitionEithers $ map simplifyIf ifs
    let (ad2, ifs2) = unzipWith (\(CondNode a' d' ifs') -> ((a',d'), ifs')) (catMaybes ad1)
    let (a', d') = foldl' ((<>)) (a,d) ad2
    CondNode a' d' (concat ifs2 ++ ifs1)
  where
    simplifyIf :: CondBranch v d a -> Either (Maybe (CondTree u d a)) (CondBranch u d a)
    simplifyIf (CondBranch cnd t me) =
        case C.simplifyCondition cnd env of
          (C.Lit True,  _) -> Left $ Just $ simplifyCondTree' env t
          (C.Lit False, _) -> Left $ fmap (simplifyCondTree' env) me
          (cnd',      _)   -> Right (CondBranch cnd' (simplifyCondTree' env t) (fmap (simplifyCondTree' env) me))

unzipWith :: (c -> (a, b)) -> [c] -> ([a], [b])
unzipWith f xs = unzip (map f xs)

-- | Simplify a configuration condition using the OS and arch names.  Returns
--   the names of all the flags occurring in the condition.
simplifyConfVar :: C.OS -> C.Arch -> C.CompilerInfo -> Map FlagName Bool -> C.ConfVar -> Either FlagName Bool
simplifyConfVar os arch cinfo mflags = interp
  where
    interp (C.OS os')    = Right $ os' == os
    interp (C.Arch arch') = Right $ arch' == arch
    interp (C.Impl comp vr)
      | matchImpl (C.compilerInfoId cinfo) = Right True
      | otherwise = case C.compilerInfoCompat cinfo of
          -- fixme: treat Nothing as unknown, rather than empty list once we
          --        support partial resolution of system parameters
          Nothing     -> Right False
          Just compat -> Right (any matchImpl compat)
          where
            matchImpl (C.CompilerId c v) = comp == c && v `withinRange` vr
    interp (C.PackageFlag f) = case Map.lookup f mflags of
        Nothing -> Left f
        Just b -> Right b
