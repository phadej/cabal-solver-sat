module Distribution.Solver.SAT.Sources (
    SourcePackageIndex (..),
    SourcePackage (..),
    lookupSourcePackage,
    readSourcePackage,
) where

import Distribution.PackageDescription.Parsec
       (parseGenericPackageDescriptionMaybe)

import Distribution.Solver.SAT.Base

import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Index as Tar
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Map.Strict         as Map

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
        Just (RemotePackage offset) -> do
            entry <- Tar.hReadEntry hdl offset
            case Tar.entryContent entry of
                Tar.NormalFile lbs _ -> do
                    gpd <- maybe (fail "foo") return (parseGenericPackageDescriptionMaybe (LBS.toStrict lbs))
                    return gpd


                _ -> do
                    fail "wront tar entry"
