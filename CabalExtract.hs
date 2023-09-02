import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.System
import Distribution.Compiler
import Distribution.Version
import Data.Maybe (catMaybes)
import Data.Either
import Distribution.Pretty (prettyShow)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Control.Monad (forM_)

demo :: FilePath -> IO ()
demo fp = do
    contents <- BS.readFile fp
    gpd <- maybe (fail "foo") return (parseGenericPackageDescriptionMaybe contents)
    let tree = fmap targetBuildDepends $ extract gpd
    go 0 tree
  where
    go n (CondNode deps _ ifs) = do
        forM_ deps $ \dep ->
            putStrLn $ replicate n ' ' ++ prettyShow dep
        forM_ ifs $ \(CondBranch c x y) -> do
            putStrLn $ replicate n ' ' ++ "if " ++ show c
            go (n + 2) x
            forM_ y $ \y' -> do
                putStrLn $ replicate n ' ' ++ "else"
                go (n + 2) y'

extract :: GenericPackageDescription -> CondTree FlagName () BuildInfo
extract gpd = case condLibrary gpd of
    Nothing -> CondNode mempty () []
    Just l  -> extract2 (manualFlags gpd) (extract1 l)

extract1 :: CondTree ConfVar [Dependency] Library -> CondTree ConfVar () BuildInfo
extract1 = mapCondTree libBuildInfo (const ()) id

extract2 :: (Semigroup c, Semigroup a) => Map FlagName Bool -> CondTree ConfVar c a -> CondTree FlagName c a
extract2 mflags = simplifyCondTree' (simplifyConfVar Linux X86_64 (CompilerInfo (CompilerId GHC (mkVersion [8,10,7])) NoAbiTag Nothing Nothing Nothing) mflags)

manualFlags :: GenericPackageDescription -> Map FlagName Bool
manualFlags gpd = Map.fromList
    [ (fn, v)
    | MkPackageFlag { flagName = fn, flagDefault = v, flagManual = True } <- genPackageFlags gpd 
    ]

simplifyCondTree'
    :: forall a d u v. (Semigroup a, Semigroup d)
    => (v -> Either u Bool)
    -> CondTree v d a
    -> CondTree u d a 
simplifyCondTree' env (CondNode a d ifs) = do
    let (ad1, ifs1) = partitionEithers $ map simplifyIf ifs
    let (ad2, ifs2) = unzipWith (\(CondNode a' d' ifs') -> ((a',d'), ifs')) (catMaybes ad1)
    let (a', d') = foldl' ((<>)) (a,d) ad2
    CondNode a' d' (concat ifs2 ++ ifs1)
  where
    simplifyIf :: CondBranch v d a -> Either (Maybe (CondTree u d a)) (CondBranch u d a) 
    simplifyIf (CondBranch cnd t me) =
        case simplifyCondition cnd env of
          (Lit True,  _) -> Left $ Just $ simplifyCondTree' env t
          (Lit False, _) -> Left $ fmap (simplifyCondTree' env) me
          (cnd',      _) -> Right (CondBranch cnd' (simplifyCondTree' env t) (fmap (simplifyCondTree' env) me))

unzipWith :: (c -> (a, b)) -> [c] -> ([a], [b])
unzipWith f xs = unzip (map f xs)

-- | Simplify a configuration condition using the OS and arch names.  Returns
--   the names of all the flags occurring in the condition.
simplifyConfVar :: OS -> Arch -> CompilerInfo -> Map FlagName Bool -> ConfVar -> Either FlagName Bool
simplifyConfVar os arch cinfo mflags = interp
  where
    interp (OS os')    = Right $ os' == os
    interp (Arch arch') = Right $ arch' == arch
    interp (Impl comp vr)
      | matchImpl (compilerInfoId cinfo) = Right True
      | otherwise = case compilerInfoCompat cinfo of
          -- fixme: treat Nothing as unknown, rather than empty list once we
          --        support partial resolution of system parameters
          Nothing     -> Right False
          Just compat -> Right (any matchImpl compat)
          where
            matchImpl (CompilerId c v) = comp == c && v `withinRange` vr
    interp (PackageFlag f) = case Map.lookup f mflags of
        Nothing -> Left f
        Just b -> Right b