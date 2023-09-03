module Distribution.Solver.SAT.Sources (
    SourcePackageIndex (..),
    SourcePackage (..),
    lookupSourcePackage,
    readSourcePackage,
) where

import Distribution.Solver.SAT.Base

import qualified Data.Map.Strict as Map

-- | Source package index, i.e. all packages to be built.
-- Includes the local packages as well (which shadow repositories).
data SourcePackageIndex = MkSourcePackageIndex
    { location :: FilePath
    , packages :: Map PackageName (Map Version SourcePackage)
    }
  deriving Show

data SourcePackage
    = ProjectPackage !GenericPackageDescription  -- ^ local, project package.
    | RemotePackage !TarEntryOffset              -- ^ package in a source-repository. tar entry offset tells which.
  deriving Show

lookupSourcePackage :: PackageName -> SourcePackageIndex -> Map Version SourcePackage
lookupSourcePackage pn idx = Map.findWithDefault Map.empty pn idx.packages

readSourcePackage :: Handle -> PackageName -> Version -> SourcePackageIndex -> IO GenericPackageDescription
readSourcePackage hdl pn pv idx = case Map.lookup pn idx.packages of
    Nothing -> fail $ "No" ++ show pn
    Just vers -> case Map.lookup pv vers of
        Nothing -> fail $ "No" ++ show (pn, pv)
        Just (ProjectPackage gpd) -> return gpd
        Just (RemotePackage offset) -> fail $ "TODO " ++ show offset
