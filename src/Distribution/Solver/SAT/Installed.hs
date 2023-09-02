module Distribution.Solver.SAT.Installed (
    InstalledPackage (..),
    InstalledPackageIndex,
    mkInstalledPackageIndex,
) where

import Distribution.Solver.SAT.Base

import qualified Data.Map as Map

data InstalledPackage = MkInstalledPackage
    { id        :: !PackageIdentifier
    , unitId    :: !UnitId
    , depends   :: !(Set UnitId)
    }
  deriving Show

instance HasField "name" InstalledPackage PackageName where
    getField ip = packageName ip.id

instance HasField "version" InstalledPackage Version where
    getField ip = packageVersion ip.id

-- | Installed package index.
--
-- indexed by unitid and map packagename (map packageindex InstalledPAckage)
data InstalledPackageIndex = MkInstalledPackageIndex
    { units :: !(Map UnitId InstalledPackage)
    , packages :: !(Map PackageName (Map Version [InstalledPackage]))
    }

mkInstalledPackageIndex :: [InstalledPackage] -> InstalledPackageIndex
mkInstalledPackageIndex pkgs = MkInstalledPackageIndex {..} where
    units    = Map.fromList [ (pkg.unitId, pkg) | pkg <- pkgs ]
    packages = Map.fromListWith (Map.unionWith (++))
        [ (pkg.name, Map.singleton pkg.version [pkg])
        | pkg <- pkgs
        ]
