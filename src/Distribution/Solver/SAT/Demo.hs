module Distribution.Solver.SAT.Demo (
    installedPackageIndex,
) where

import Distribution.Solver.SAT.Base
import Distribution.Solver.SAT
import Distribution.Solver.SAT.Installed

installedPackageIndex :: InstalledPackageIndex
installedPackageIndex = mkInstalledPackageIndex
    [ MkInstalledPackage (PackageIdentifier (mkPackageName "base") (mkVersion [4,17,1,0])) (mkUnitId "base") mempty
    ]