module Distribution.Solver.SAT.Base (module X) where

import Codec.Archive.Tar.Index   as X (TarEntryOffset)
import Control.Monad             as X (forM, forM_, unless, when)
import Control.Monad.IO.Class    as X (liftIO)
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
       (CondBranch (..), CondTree (..), Dependency (..), FlagAssignment,
       FlagName, GenericPackageDescription (..), LibraryName (..),
       PackageIdentifier (..), PackageName, mkPackageName)

import Distribution.Types.DependencyMap as X
       (DependencyMap, fromDepMap, toDepMap)
