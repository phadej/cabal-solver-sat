module Distribution.Solver.SAT.Demo (
    -- * Demo
    demo,
    demoThis,
    -- * Demo inputs
    demoInstalledPackageIndex,
    demoPlatform,
    demoCompilerInfo,
    demoPkgConfigDb,
    demoPackagePreferences,
    demoPackageConstraints,
) where

import Distribution.PackageDescription.Parsec
       (parseGenericPackageDescriptionMaybe)

import Distribution.Solver.SAT
import Distribution.Solver.SAT.Base

import qualified Cabal.Index                     as CI
import qualified Data.ByteString                 as BS
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Distribution.Compiler           as C
import qualified Distribution.PackageDescription as C
import qualified Distribution.System             as C

-- | Installed package index with @base-4.17.1.0@
demoInstalledPackageIndex :: InstalledPackageIndex
demoInstalledPackageIndex = mkInstalledPackageIndex
    [ MkInstalledPackage (PackageIdentifier (mkPackageName "base") (mkVersion [4,17,1,0])) (mkUnitId "base") mempty
    -- TODO: template-haskell...
    -- , MkInstalledPackage (PackageIdentifier (mk
    ]

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
            & Map.delete (mkPackageName "base")
            & Map.delete (mkPackageName "template-haskell")

            -- remove the target package name from the source index.
            & Map.delete pn

    printf "SourcePackageIndex at: %s\n" demoSourcePackageIndex.location
    printf "SourcePackageIndex size: %d\n" (Map.size demoSourcePackageIndex.packages)

    satSolver
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
    forM_ resolved $ \case
        Preinstalled ip -> print ip
        FromSource pi lns flags -> print (pi, lns, flags)

mkSourcePackages :: CI.PackageInfo -> Map Version SourcePackage
mkSourcePackages = Map.map mkSourcePackage . CI.piVersions

mkSourcePackage :: CI.ReleaseInfo -> SourcePackage
mkSourcePackage = RemotePackage . CI.riTarOffset

gpdPackageName :: GenericPackageDescription -> PackageName
gpdPackageName = packageName . C.package . packageDescription