module Distribution.Solver.SAT.Demo (
    -- * Demo
    demo,
    -- * Demo inputs
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
demo :: FilePath -> IO ()
demo cbl = do
    (hackageTar, _map) <- CI.cachedHackageMetadata
    print cbl
    print hackageTar
