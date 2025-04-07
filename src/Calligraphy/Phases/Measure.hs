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

import Calligraphy.Util.Printer
import Calligraphy.Util.Types

import Control.Monad
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as EnumSet
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
import Text.Printf

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
    moduleContents :: String -> Set Key,
    moduleIndex :: Key -> String,
    tableOfDependents :: Key -> Set Key
  }

prepareCache :: CallGraph -> Cache
prepareCache callGraph =
  Cache
    { allKeys = findAllKeys callGraph,
      moduleContents = (makeModuleContents callGraph Map.!),
      moduleIndex = (makeModuleIndex callGraph EnumMap.!),
      tableOfDependents = (makeTableOfDependents callGraph EnumMap.!)
    }

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

data Statistics = Statistics
  { averageTangledness, standardDeviationOfTangledness :: !Float,
    nodeCount :: !Word
  }
  deriving (Show, Eq, Ord)

data Measurements = Measurements
  { tangledness :: EnumMap Key Tangledness,
    statistics :: Statistics
  }
  deriving (Show, Eq, Ord)

measure :: CallGraph -> Measurements
measure callGraph =
  let cache@Cache {..} = prepareCache callGraph
   in let
        nodeCount = cardinality allKeys
        preliminaryTanglednessMap = EnumMap.fromSet (measurePreliminaryTangledness cache) allKeys
        tanglednessQuotientMap = EnumMap.map (.tanglednessQuotient) preliminaryTanglednessMap
        mean = sum tanglednessQuotientMap / word2Float nodeCount
        variance =
          sum (fmap (\tangledness -> (tangledness - mean) ^ (2 :: Word)) tanglednessQuotientMap)
            / (word2Float nodeCount - 1) -- note that we use Bessel correction when computing variance
        statistics =
          Statistics
            { averageTangledness = mean,
              standardDeviationOfTangledness = sqrt variance,
              ..
            }
        highest = word2Float nodeCount
        normalizer = makeBetaDistributionCumulativeFunction 1 highest mean variance
        tangledness = flip fmap preliminaryTanglednessMap $ \PreliminaryTangledness {..} ->
          let normalizedTangledness = normalizer ((tanglednessQuotient - 1) / highest) * 2 - 1
           in Tangledness {..}
       in
        Measurements {..}

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
    mustBeRecompiledSet = Set.insert key (tableOfDependents key)
    willBeRecompiledSet = foldMap (moduleContents . moduleIndex) mustBeRecompiledSet
    mustBeRecompiled = cardinality mustBeRecompiledSet
    willBeRecompiled = cardinality willBeRecompiledSet
    tanglednessQuotient = word2Float willBeRecompiled / word2Float mustBeRecompiled
   in
    PreliminaryTangledness {..}

ppMeasurements :: Prints Measurements
ppMeasurements Measurements {..} = do
  showLn statistics
  forM_ (EnumMap.toList tangledness) showLn

ppStatistics :: Prints Statistics
ppStatistics Statistics {..} = do
  strLn $ "node count" <:> nodeCount
  strLn $ "average tangledness" <:> NiceFloat averageTangledness
  strLn $ "standard deviation" <:> NiceFloat standardDeviationOfTangledness

ppCache :: Prints Cache
ppCache Cache {..} = do
  showLn allKeys
  mapM_ showLn $ EnumMap.fromSet moduleIndex allKeys
  mapM_ showLn $ Map.fromSet moduleContents (Set.map moduleIndex ((Set.fromAscList . EnumSet.toAscList) allKeys))
  mapM_ showLn $ EnumMap.fromSet tableOfDependents allKeys

class Cardinality number container where
  cardinality :: container -> number

instance {-# OVERLAPPABLE #-} (Foldable foldable, Num number) => Cardinality number (foldable α) where
  cardinality = fromIntegral . length

instance (Num number) => Cardinality number (EnumSet α) where
  cardinality = fromIntegral . EnumSet.size

(<:>) :: (Show values) => String -> values -> String
name <:> value = name <> ": " <> show value
infix 6 <:>

newtype NiceFloat = NiceFloat Float
instance Show NiceFloat where show (NiceFloat float) = printf "%.3f" float
