module Distribution.Solver.SAT.Base (
    module X,
    prettyLibraryName,
    bold, blue, green, printSection, printSubsection,
) where

import Codec.Archive.Tar.Index   as X (TarEntryOffset)
import Control.Monad             as X (forM, forM_, unless, when)
import Control.Monad.IO.Class    as X (MonadIO (..))
import Control.Monad.Trans.Class as X (MonadTrans (..))
import Data.Either               as X (partitionEithers)
import Data.Foldable             as X (foldl', toList)
import Data.Foldable.WithIndex   as X (ifor_)
import Data.Function             as X ((&))
import Data.Functor.Identity     as X (Identity (..))
import Data.Map                  as X (Map)
import Data.Maybe                as X (catMaybes, fromMaybe)
import Data.Set                  as X (Set)
import Data.Void                 as X (Void)
import Distribution.Package      as X (packageName, packageVersion)
import Data.List.NonEmpty as X (NonEmpty (..), nonEmpty)
import Distribution.Pretty       as X (prettyShow)
import Distribution.Types.UnitId as X (UnitId, mkUnitId)
import Distribution.Version      as X
       (Version, VersionRange, mkVersion, withinRange)
import GHC.Generics              as X (Generic)
import GHC.Records               as X (HasField (..))
import Prelude                   as X hiding (pi)
import System.IO                 as X (Handle, IOMode (..), withFile)
import Text.Printf               as X (printf)

import Distribution.PackageDescription as X
       (CondBranch (..), CondTree (..), Condition (..), Dependency (..),
       FlagAssignment, FlagName, GenericPackageDescription (..),
       LibraryName (..), PackageIdentifier (..), PackageName, mkFlagAssignment, mkPackageName)

import Distribution.Types.DependencyMap as X
       (DependencyMap, fromDepMap, toDepMap)

import qualified System.Console.ANSI as ANSI

prettyLibraryName :: LibraryName -> String
prettyLibraryName LMainLibName = "lib"
prettyLibraryName (LSubLibName c) = "lib:" ++ prettyShow c

bold :: String -> String
bold str = ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity] ++ str ++ ANSI.setSGRCode []

blue :: String -> String
blue str = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Blue] ++ str ++ ANSI.setSGRCode []

green :: String -> String
green str = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green] ++ str ++ ANSI.setSGRCode []

printSection :: MonadIO m => String -> m ()
printSection s = liftIO $ do
    putStrLn ""
    putStrLn $ bold s
    putStrLn "=============================================="

printSubsection :: MonadIO m => String -> m ()
printSubsection s = liftIO $ do
    putStrLn ""
    putStrLn $ bold s
    putStrLn "----------------------------------------------"
