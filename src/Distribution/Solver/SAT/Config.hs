module Distribution.Solver.SAT.Config (
    Config (..),
) where

import Distribution.Solver.SAT.Base

data Config = MkConfig
    { maxIterations :: !Int
    , reverse       :: !Bool
    , improve       :: !Int
    , printModels   :: !Bool
    , printStats    :: !Bool
    }
  deriving Show
