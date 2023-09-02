module Distribution.Solver.SAT.Demo (
    demo,
    demoInstalledPackageIndex,
    demoPlatform,
    demoCompilerInfo,
    demoPkgConfigDb,
    demoPackagePreferences,
    demoPackageConstraints,
) where

import Distribution.Solver.SAT
import Distribution.Solver.SAT.Base

import qualified Cabal.Index as CI
import qualified Distribution.Compiler as C
import qualified Distribution.System   as C

-- | Installed package index with @base-4.17.1.0@
demoInstalledPackageIndex :: InstalledPackageIndex
demoInstalledPackageIndex = mkInstalledPackageIndex
    [ MkInstalledPackage (PackageIdentifier (mkPackageName "base") (mkVersion [4,17,1,0])) (mkUnitId "base") mempty
    ]

demoPlatform :: C.Platform
demoPlatform = C.Platform C.X86_64 C.Linux

demoCompilerInfo :: C.CompilerInfo
demoCompilerInfo = C.CompilerInfo (C.CompilerId C.GHC (mkVersion [8,10,7])) C.NoAbiTag Nothing Nothing Nothing

demoPkgConfigDb :: PkgConfigDb
demoPkgConfigDb = MkPkgConfigDb

demoPackagePreferences :: PackagePreferences
demoPackagePreferences = MkPackagePreferences

demoPackageConstraints :: PackageConstraints
demoPackageConstraints = MkPackageConstraints

demo :: FilePath -> IO ()
demo cbl = do
    (hackageTar, _map) <- CI.cachedHackageMetadata
    print cbl
    print hackageTar
