module Distribution.Solver.SAT.Demo (
    -- * Demo
    demo,
    demoThis,
    demoSudoku,
    demoPandoc,
    -- * Demo inputs
    demoConfig,
    demoInstalledPackageIndex,
    demoPlatform,
    demoCompilerInfo,
    demoPkgConfigDb,
    demoPackagePreferences,
    demoPackageConstraints,
) where

import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)

import Distribution.Solver.SAT
import Distribution.Solver.SAT.Base

import qualified Cabal.Index                     as CI
import qualified Codec.Archive.Tar.Entry         as Tar
import qualified Codec.Archive.Tar.Index         as Tar
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Distribution.Compiler           as C
import qualified Distribution.PackageDescription as C
import qualified Distribution.System             as C

-- | Demo configuration.
demoConfig :: Config
demoConfig = MkConfig
    { maxIterations = 100
    , reverse       = True
    , improve       = 30
    , printModels   = False
    , printStats    = True
    }

-- | Installed package index with @base-4.17.1.0@
demoInstalledPackageIndex :: InstalledPackageIndex
demoInstalledPackageIndex = mkInstalledPackageIndex
    [ mk "base"               [4,17,1,0]
    , mk "ghc-bignum"         [1,3]
    , mk "ghc-prim"           [0,9,1]
    , mk "integer-gmp"        [1,1]
    , mk "system-cxx-std-lib" [1,0]
    , mk "template-haskell"   [2,19,0,0]

    , mk "sat-simple" [0,1,0,0]
    ]
  where
    mk name digits = MkInstalledPackage (PackageIdentifier (mkPackageName name) (mkVersion digits)) (mkUnitId name) mempty

-- | Demo platform is x86 64bit Linux.
demoPlatform :: C.Platform
demoPlatform = C.Platform C.X86_64 C.Linux

-- | Demo compiler is GHC-9.4.7
--
-- Note: we don't specify supported languages or extensions.
-- For now we assume that all extensions/languages are supported.
--
demoCompilerInfo :: C.CompilerInfo
demoCompilerInfo = C.CompilerInfo (C.CompilerId C.GHC (mkVersion [9,4,7])) C.NoAbiTag Nothing Nothing Nothing

-- | Demo package db.
demoPkgConfigDb :: PkgConfigDb
demoPkgConfigDb = MkPkgConfigDb

-- | Demo package preferences: no preferences.
demoPackagePreferences :: PackagePreferences
demoPackagePreferences = MkPackagePreferences

-- | Demo package constraints: no package constraints.
demoPackageConstraints :: PackageConstraints
demoPackageConstraints = MkPackageConstraints

-- | Demo. Solve for an input @.cabal@ file.
demo :: FilePath -> IO [ResolvedPackage DemoLoc]
demo cabalFile = do
    contents <- BS.readFile cabalFile
    gpd <- maybe (fail "foo") return (parseGenericPackageDescriptionMaybe contents)
    let pn = gpdPackageName gpd
        pv = gpdPackageVersion gpd
    printf "Solving for %s\n" (prettyShow pn)

    -- constructing source package index
    (hackageTar, hackage) <- CI.cachedHackageMetadata

    let demoSourcePackageIndex :: SourcePackageIndex DemoLoc
        demoSourcePackageIndex = mkDemoSourcePackageIndex hackageTar $
            Map.map mkSourcePackages hackage

            -- remove few non-re-installable packages.
            -- These are hardcoded in cabal-install as well.
            & Map.delete (mkPackageName "ghc")
            & Map.delete (mkPackageName "ghc-prim")
            & Map.delete (mkPackageName "ghc-bignum")
            & Map.delete (mkPackageName "integer-gmp")
            & Map.delete (mkPackageName "base")
            & Map.delete (mkPackageName "template-haskell")

            -- remove the target package name from the source index.
            & Map.insert pn (Map.singleton pv (ProjectPackage gpd))

    printf "SourcePackageIndex size: %d\n" (case demoSourcePackageIndex of MkSourcePackageIndex _ packages -> Map.size packages)

    satSolver
        demoConfig
        demoPlatform
        demoCompilerInfo
        demoInstalledPackageIndex
        demoSourcePackageIndex
        demoPkgConfigDb
        demoPackagePreferences
        demoPackageConstraints
        (Set.singleton (pn, CLibName LMainLibName))

demoThis :: IO ()
demoThis = do
    resolved <- demo "cabal-solver-sat.cabal"
    printSolution resolved

-- | https://oleg.fi/gists/posts/2023-08-30-using-cabal-install-solver-as-sat-solver.html
demoSudoku :: IO ()
demoSudoku = do
    resolved <- demo "examples/sudoku.cabal"
    printSolution resolved

demoPandoc :: IO ()
demoPandoc = do
    resolved <- demo "examples/pandoc-3.1.7.cabal"
    printSolution resolved

printSolution :: [ResolvedPackage DemoLoc] -> IO ()
printSolution resolved = do
    printSection "Solution"

    forM_ resolved $ \case
        Preinstalled ip -> putStrLn $ green $ prettyShow ip.id
        FromSource loc pi lns flags -> putStrLn $ unwords $
            [ (case loc of Local -> cyan; Hackage -> id) $ prettyShow pi ] ++
            [ prettyShow ln | ln <- toList lns ] ++
            [ prettyShow flags ]

mkSourcePackages :: CI.PackageInfo -> Map Version DemoSourcePackage
mkSourcePackages = Map.map mkSourcePackage . CI.piVersions

mkSourcePackage :: CI.ReleaseInfo -> DemoSourcePackage
mkSourcePackage = RemotePackage . CI.riTarOffset

gpdPackageName :: GenericPackageDescription -> PackageName
gpdPackageName = packageName . C.package . packageDescription

gpdPackageVersion :: GenericPackageDescription -> Version
gpdPackageVersion = packageVersion . C.package . packageDescription

data DemoLoc = Local | Hackage deriving Show

data DemoSourcePackage
    = ProjectPackage !GenericPackageDescription  -- ^ local, project package.
    | RemotePackage !TarEntryOffset              -- ^ package in a source-repository. tar entry offset tells which.
  deriving Show

mkDemoSourcePackageIndex :: FilePath -> Map PackageName (Map Version DemoSourcePackage) -> SourcePackageIndex DemoLoc
mkDemoSourcePackageIndex tarPath index = MkSourcePackageIndex open index where
    open :: ((DemoSourcePackage -> IO (SourcePackage DemoLoc)) -> IO r) -> IO r
    open kont = withFile tarPath ReadMode $ \hdl -> do
        kont (readSrcPkg hdl)

    readSrcPkg :: Handle -> DemoSourcePackage -> IO (SourcePackage DemoLoc)
    readSrcPkg _   (ProjectPackage gpd)   = return (MkSourcePackage Local gpd)
    readSrcPkg hdl (RemotePackage offset) = do
        entry <- Tar.hReadEntry hdl offset
        case Tar.entryContent entry of
            Tar.NormalFile lbs _ -> do
                gpd <- maybe (fail "foo") return (parseGenericPackageDescriptionMaybe (LBS.toStrict lbs))
                return (MkSourcePackage Hackage gpd)

            _ -> do
                fail "wrong tar entry"
