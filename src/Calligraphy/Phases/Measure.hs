{-# HLINT ignore "Redundant id" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Calligraphy.Phases.Measure where

import Calligraphy.Util.Assorti
import Calligraphy.Util.Printer
import Calligraphy.Util.Types

import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as EnumSet
import qualified Data.Foldable as Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple
import GHC.Float
import Options.Applicative
import Statistics.Distribution
import Statistics.Distribution.Beta

data MeasurementConfig = MeasurementConfig
  { doMeasure :: Bool,
    doPrintStatistics :: Bool
  }

pMeasurementConfig :: Parser MeasurementConfig
pMeasurementConfig = do
  doMeasure <- switch (long "measure" <> help "…") -- TODO write help message
  doPrintStatistics <- switch (long "print-statistics" <> help "…") -- TODO write help message
  pure MeasurementConfig {..}

findAllKeys :: CallGraph -> EnumSet Key
findAllKeys = foldMap folding . _modules
  where
    folding = (foldMap . foldMap) (EnumSet.singleton . declKey) . moduleForest

makeModuleContents :: CallGraph -> Map String (Set Key)
makeModuleContents (CallGraph modules _ _) = foldMap moduleFolding modules
  where
    moduleFolding modulus =
      Map.singleton
        (moduleName modulus)
        (((foldMap . foldMap) (Set.singleton . declKey) . moduleForest) modulus)

makeModuleIndex :: CallGraph -> EnumMap Key String
makeModuleIndex =
  id
    . EnumMap.fromList
    . fmap swap
    . concatMap (traverse Set.toList)
    . Map.toList
    . makeModuleContents

makeTableOfDependents :: CallGraph -> EnumMap Key (Set Key)
makeTableOfDependents callGraph@(CallGraph _ calls types) =
  (enumifyMap . fmap (fromMaybe Set.empty))
    (Map.fromSet (flip Map.lookup ((makeTableFromSet . Set.map swap) dependentsRelation)) (denumifySet (findAllKeys callGraph)))
  where
    enumifyMap = EnumMap.fromAscList . Map.toAscList
    denumifySet = Set.fromAscList . EnumSet.toAscList
    dependentsRelation :: Set (Key, Key)
    dependentsRelation = calls <> types
    makeTableFromSet :: Set (Key, Key) -> Map Key (Set Key)
    makeTableFromSet =
      Map.mapKeysWith (<>) fst
        . Map.fromSet (Set.singleton . snd)

data Cache = Cache
  { allKeys :: EnumSet Key,
    allModuleNames :: Set String,
    moduleContents :: String -> Set Key,
    moduleIndex :: Key -> String,
    dependents :: Key -> Set Key
  }

prepareCache :: CallGraph -> Cache
prepareCache callGraph =
  Cache
    { allKeys = findAllKeys callGraph,
      allModuleNames = Map.keysSet moduleContentsMap,
      moduleContents = (moduleContentsMap Map.!),
      moduleIndex = (makeModuleIndex callGraph EnumMap.!),
      dependents = (makeTableOfDependents callGraph EnumMap.!)
    }
  where
    moduleContentsMap = makeModuleContents callGraph

-- | This type is used to hold the intermedate result where tangledness quotient
-- is already computed for each node but the statistics needed for normalization
-- are not yet computed.
data PreliminaryTangledness = PreliminaryTangledness
  { willBeRecompiled, mustBeRecompiled :: !Word,
    tanglednessQuotient :: !Float
  }
  deriving (Show, Eq, Ord)

data Tangledness = Tangledness
  { willBeRecompiled :: !Word,
    mustBeRecompiled :: !Word,
    tanglednessQuotient :: !Float,
    -- | Normalized tangledness is more or less uniformly distributed in
    -- the interval \([-1 … 1]\), smaller values indicating higher
    -- tangledness.
    normalizedTangledness :: !Float
  }
  deriving (Show, Eq, Ord)

data Cohesion = Cohesion
  { trueCohesion :: !Float,
    normalizedCohesion :: !Float
  }
  deriving (Show, Eq, Ord)

data Statistics = Statistics
  { averageTangledness, standardDeviationOfTangledness :: !Float,
    nodeCount :: !Word,
    averageCohesion, standardDeviationOfCohesion :: !Float,
    moduleCount :: !Word
  }
  deriving (Show, Eq, Ord)

data Measurements = Measurements
  { tangledness :: EnumMap Key Tangledness,
    cohesion :: Map String Cohesion,
    statistics :: Statistics
  }
  deriving (Show, Eq, Ord)

measure :: CallGraph -> Measurements
measure callGraph =
  let
    cache = prepareCache callGraph
    (nodeCount, averageTangledness, standardDeviationOfTangledness, tangledness) = measureTangledness cache
    (moduleCount, averageCohesion, standardDeviationOfCohesion, cohesion) = measureCohesion cache
    statistics = Statistics {..}
   in
    Measurements {..}

measureTangledness :: Cache -> (Word, Float, Float, EnumMap Key Tangledness)
measureTangledness cache@Cache {..} =
  let
    nodeCount = cardinality allKeys
    preliminaryTanglednessMap = EnumMap.fromSet (measurePreliminaryTangledness cache) allKeys
    tanglednessQuotientMap = EnumMap.map (.tanglednessQuotient) preliminaryTanglednessMap
    mean = sum tanglednessQuotientMap / word2Float nodeCount
    variance =
      sigma tanglednessQuotientMap (\tangledness -> (tangledness - mean) ** 2)
        / (word2Float nodeCount - 1) -- note that we use Bessel correction when computing variance
    highest = word2Float nodeCount
    normalizer =
      if variance /= 0
        then makeBetaDistributionCumulativeFunction 1 highest mean variance
        else const 0.5 -- because our beta distribution would be undefined when variance is zero
    tangledness = flip fmap preliminaryTanglednessMap $ \PreliminaryTangledness {..} ->
      let normalizedTangledness = negate (normalizer ((tanglednessQuotient - 1) / highest) * 2 - 1)
       in Tangledness {..}
   in
    (nodeCount, mean, sqrt variance, tangledness)

measureCohesion :: Cache -> (Word, Float, Float, Map String Cohesion)
measureCohesion Cache {..} =
  let
    moduleCount = cardinality allModuleNames
    cohesionMap = flip Map.fromSet allModuleNames $ \moduleName ->
      let nodes = moduleContents moduleName
       in if null nodes
            then 0.5 -- because cohesion is really undefined for empty modules but we do not want to crash the program
            else 1 / cardinality nodes + 1 / cardinality nodes ** 2 * sigma nodes (cardinality . Set.intersection nodes . dependents)
    mean = sum cohesionMap / word2Float moduleCount
    variance =
      sigma cohesionMap (\cohesion -> (cohesion - mean) ** 2)
        / (word2Float moduleCount - 1) -- note that we use Bessel correction when computing variance
    normalizer =
      if variance /= 0
        then makeBetaDistributionCumulativeFunction 0 1 mean variance
        else const 0.5 -- because our beta distribution would be undefined when variance is zero
    cohesion = flip fmap cohesionMap $ \trueCohesion ->
      let normalizedCohesion = normalizer trueCohesion * 2 - 1
       in Cohesion {..}
   in
    (moduleCount, mean, sqrt variance, cohesion)

-- | Does the same as the mathematical Σ big operator.
sigma :: (Foldable foldable, Num number) => foldable α -> (α -> number) -> number
sigma set function = (sum . fmap function) (Foldable.toList set)

-- | Compute the cumulative distribution function of the beta distribution given
-- by the minimum, maximum, mean and variance provided.
--
-- See <https://en.wikipedia.org/wiki/Beta_distribution#Method_of_moments> for
-- details.
makeBetaDistributionCumulativeFunction :: Float -> Float -> Float -> Float -> Float -> Float
makeBetaDistributionCumulativeFunction lowest highest mean variance =
  let
    a = lowest
    c = highest
    x = (mean - a) / (c - a)
    v = variance / (c - a) ** 2
    t = (x * (1 - x) / v - 1)
    α = x * t
    β = (1 - x) * t
    distribution = betaDistr (float2Double α) (float2Double β)
   in
    (double2Float . cumulative distribution . float2Double)

measurePreliminaryTangledness :: Cache -> Key -> PreliminaryTangledness
measurePreliminaryTangledness Cache {..} key =
  let
    mustBeRecompiledSet = Set.insert key (dependents key)
    willBeRecompiledSet = foldMap (moduleContents . moduleIndex) mustBeRecompiledSet
    mustBeRecompiled = cardinality mustBeRecompiledSet
    willBeRecompiled = cardinality willBeRecompiledSet
    tanglednessQuotient = word2Float willBeRecompiled / word2Float mustBeRecompiled
   in
    PreliminaryTangledness {..}

ppMeasurements :: Prints Measurements
ppMeasurements Measurements {..} = do
  strLn "measurements:"
  indent $ do
    strLn "statistics:"
    indent $ ppStatistics statistics
    strLn "tangledness:"
    indent $ mapM_ showLn $ EnumMap.toList tangledness
    strLn "cohesion:"
    indent $ mapM_ showLn $ Map.toList cohesion

ppStatistics :: Prints Statistics
ppStatistics Statistics {..} = do
  strLn "statistics:"
  indent $ do
    strLn $ "node count" <:> nodeCount
    strLn $ "average tangledness" <:> NiceFloat averageTangledness
    strLn $ "standard deviation" <:> NiceFloat standardDeviationOfTangledness
    strLn $ "module count" <:> moduleCount
    strLn $ "average cohesion" <:> Percent averageCohesion
    strLn $ "standard deviation" <:> Percent standardDeviationOfCohesion

ppCache :: Prints Cache
ppCache Cache {..} = do
  strLn "cache:"
  indent $ do
    strLn "all keys:"
    indent $ showLn allKeys
    strLn "module index:"
    indent $ mapM_ showLn $ EnumMap.toList (EnumMap.fromSet moduleIndex allKeys)
    strLn "module contents"
    indent $ mapM_ showLn $ Map.toList (Map.fromSet moduleContents (Set.map moduleIndex ((Set.fromAscList . EnumSet.toAscList) allKeys)))
    strLn "dependents"
    indent $ mapM_ showLn $ EnumMap.toList (EnumMap.fromSet dependents allKeys)

class Cardinality number container where
  cardinality :: container -> number

instance {-# OVERLAPPABLE #-} (Foldable foldable, Num number) => Cardinality number (foldable α) where
  cardinality = fromIntegral . length

instance (Num number) => Cardinality number (EnumSet α) where
  cardinality = fromIntegral . EnumSet.size
