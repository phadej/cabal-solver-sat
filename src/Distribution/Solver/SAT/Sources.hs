module Distribution.Solver.SAT.Sources (
    SourcePackageIndex (..),
    OpenedSourcePackageIndex,
    SourcePackage (..),
    openSourcePackageIndex,
    lookupSourcePackage,
    readSourcePackage,
) where

import Distribution.Solver.SAT.Base

import qualified Data.Map.Strict as Map

data SourcePackageIndex loc where
    MkSourcePackageIndex
        :: (forall r. ((s -> IO (SourcePackage loc)) -> IO r) -> IO r)  -- ^ open source local package index
        -> Map PackageName (Map Version s)
        -> SourcePackageIndex loc

data SourcePackage loc = MkSourcePackage
    { location    :: !loc
    , description :: !GenericPackageDescription
    }
  deriving Show

data OpenedSourcePackageIndex srcpkg loc = MkOpenedSourcePackageIndex
    { packages :: Map PackageName (Map Version srcpkg)
    , lookup   :: srcpkg -> IO (SourcePackage loc)
    }

openSourcePackageIndex :: SourcePackageIndex loc -> (forall srcpkg. OpenedSourcePackageIndex srcpkg loc -> IO r) -> IO r
openSourcePackageIndex (MkSourcePackageIndex o m) kont = o (\l -> kont (MkOpenedSourcePackageIndex m l))

lookupSourcePackage :: PackageName -> OpenedSourcePackageIndex srcpkg loc -> Map Version srcpkg
lookupSourcePackage pn idx = Map.findWithDefault Map.empty pn idx.packages

readSourcePackage :: PackageName -> Version -> OpenedSourcePackageIndex srcpkg loc -> IO (SourcePackage loc)
readSourcePackage pn pv idx = case Map.lookup pn idx.packages of
    Nothing -> fail $ "No" ++ show pn
    Just vers -> case Map.lookup pv vers of
        Nothing -> fail $ "No" ++ show (pn, pv)
        Just srcpkg -> idx.lookup srcpkg
