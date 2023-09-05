module Distribution.Solver.SAT.Demo (
    -- * Demo
    demo,
    demoThis,
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
import qualified Data.ByteString                 as BS
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
    , improve       = 10
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
demo :: FilePath -> IO [ResolvedPackage]
demo cabalFile = do
    contents <- BS.readFile cabalFile
    gpd <- maybe (fail "foo") return (parseGenericPackageDescriptionMaybe contents)
    let pn = gpdPackageName gpd
        pv = gpdPackageVersion gpd
    printf "Solving for %s\n" (prettyShow pn)

    -- constructing source package index
    (hackageTar, hackage) <- CI.cachedHackageMetadata

    let demoSourcePackageIndex :: SourcePackageIndex
        demoSourcePackageIndex = MkSourcePackageIndex hackageTar $
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

    printf "SourcePackageIndex at: %s\n" demoSourcePackageIndex.location
    printf "SourcePackageIndex size: %d\n" (Map.size demoSourcePackageIndex.packages)

    satSolver
        demoConfig
        demoPlatform
        demoCompilerInfo
        demoInstalledPackageIndex
        demoSourcePackageIndex
        demoPkgConfigDb
        demoPackagePreferences
        demoPackageConstraints
        (Set.singleton pn)

demoThis :: IO ()
demoThis = do
    resolved <- demo "cabal-solver-sat.cabal"
    printSection "Solution"

    forM_ resolved $ \case
        Preinstalled ip -> putStrLn $ green $ prettyShow ip.id
        FromSource pi lns flags -> putStrLn $ unwords $
            [ prettyShow pi ] ++
            [ prettyLibraryName ln | ln <- toList lns ] ++
            [ prettyShow flags ]

mkSourcePackages :: CI.PackageInfo -> Map Version SourcePackage
mkSourcePackages = Map.map mkSourcePackage . CI.piVersions

mkSourcePackage :: CI.ReleaseInfo -> SourcePackage
mkSourcePackage = RemotePackage . CI.riTarOffset

gpdPackageName :: GenericPackageDescription -> PackageName
gpdPackageName = packageName . C.package . packageDescription

gpdPackageVersion :: GenericPackageDescription -> Version
gpdPackageVersion = packageVersion . C.package . packageDescription
