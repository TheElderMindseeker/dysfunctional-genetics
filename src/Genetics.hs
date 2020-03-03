module Genetics
    (
      Gene (Dominant, Mixed, Recessive)
    , Genotype
    , Ratio
    , firstGeneration
    , firstGenerationRatio
    ) where

import qualified Data.Map as Map
import Data.Maybe

-- | A type representing a single gene.
-- In genetics, a gene consists of two alleles which can be in two states:
-- dominant or recessive. As the sequence is not important, a gene can be in
-- three states: dominant (AA), mixed (Aa), or recessive (aa).
data Gene = Dominant
          | Mixed
          | Recessive deriving (Show, Eq, Ord)

-- instance Ord Gene where
--     compare fGene sGene = compare (dominance fGene) (dominance sGene)

-- | Genotype is a list of genes.
-- Genes in a genotype must contain different alleles (with different IDs).
type Genotype = [Gene]

-- | Convenience type used in generation computing functions.
type Ratio = (Genotype, Integer)

-- | Helper function for calculating crossing ratios.
dominance :: Gene -> Integer
dominance Dominant = 2
dominance Mixed = 1
dominance Recessive = 0

-- | Normalizes ratios such that all of them are mutually prime.
normalizeRatios :: [(any, Integer)]
                -> [(any, Integer)]
normalizeRatios ratios = map divideByGCD ratios
  where
    divideByGCD (anything, ratio) = (anything, ratio `div` ratioGcd)
    ratioGcd = foldl1 gcd (map snd ratios)

-- | Performs a crossing of a single type of gene.
-- This operation can be thought of as the first generation of the crossing of
-- one-gene organisms.
crossTwoGenes :: (Gene, Gene)       -- ^ Pair of genes to cross
              -> [(Gene, Integer)]  -- ^ Ratios of children genes
crossTwoGenes (fGene, sGene) = normalizeRatios filteredRatios
  where
    filteredRatios = filter zeroRatio [dominant, mixed, recessive]
    dominant = (Dominant, fDom * sDom)
    mixed = (Mixed, fDom * sRec + fRec * sDom)
    recessive = (Recessive, fRec * sRec)
    fDom = dominance fGene
    fRec = 2 - fDom
    sDom = dominance sGene
    sRec = 2 - sDom
    zeroRatio (_, ratio) = ratio /= 0

-- | Helper function for generating children ratio.
-- The function takes a single gene and prepends all the genotypes in the list
-- of ratios with that gene. The function also modifies the ratio coefficient
-- of each ratio by multiplying it by gene's ratio.
prependGenotype :: (Gene, Integer)  -- ^ Gene to prepend and its ratio
                -> [Ratio]          -- ^ List of ratios to update
                -> [Ratio]          -- ^ List of updated ratios
prependGenotype (gene, geneRatio) genotypeRatios = map prepend genotypeRatios
  where
    prepend (gt, gtRatio) = (gene : gt, geneRatio * gtRatio)

-- | Helper function for generating genotype ratios.
-- The construction of the genotype means a single choice of each of the unique
-- genes comprising the organism.
constructGenotypes :: [[(Gene, Integer)]]  -- ^ Genes to construct genotypes
                   -> [Ratio]              -- ^ Constructed genotypes with ratio
constructGenotypes [] = [([], 1)]
constructGenotypes (headGenes:remGenes) = concatMap prependGenotypes headGenes
  where
    prependGenotypes :: (Gene, Integer) -> [Ratio]
    prependGenotypes gene = prependGenotype gene ratios
    ratios = constructGenotypes remGenes

-- | Calculates the ratios of genotypes in the first generation.
firstGeneration :: Genotype  -- ^ Genotype of the first parent
                -> Genotype  -- ^ Genotype of the second parent
                -> [Ratio]   -- ^ List of children genotypes with ratios
firstGeneration firstParent secondParent = normalizeRatios genotypes
  where
    genotypes = constructGenotypes crossedGenes
    crossedGenes = map crossTwoGenes genePairs
    genePairs = zip firstParent secondParent

-- | Helper function for multiplying all ratios by an integer.
multiplyRatio :: ([Ratio], Integer)  -- ^ Ratios and integer to multiply by
              -> [Ratio]             -- ^ Updated ratios
multiplyRatio (ratios, multiplier) = map multiply ratios
  where
    multiply (gene, geneRatio) = (gene, geneRatio * multiplier)

-- | Helper function for merging ratios.
mergeRatios :: [Ratio]  -- ^ First ratio list
            -> [Ratio]  -- ^ Second ratio list
            -> [Ratio]  -- ^ Merged ratios
mergeRatios baseList [] = baseList
mergeRatios baseList (x:xs) = mergeRatios (Map.toList extendedList) xs
  where
    extendedList = if isJust gtRatio
                   then Map.insert genotype (fromJust gtRatio + ratio) baseMap
                   else Map.insert genotype ratio baseMap
    gtRatio = Map.lookup genotype baseMap
    baseMap = Map.fromList baseList
    (genotype, ratio) = x

-- | Calculates the ratios of genotypes in the first generation.
firstGenerationRatio :: Ratio    -- ^ Ratio of the first parent
                     -> Ratio    -- ^ Ratio of the second parent
                     -> [Ratio]  -- ^ List of children genotypes with ratios
firstGenerationRatio (fParent, fRatio) (sParent, sRatio) = result
  where
    result = normalizeRatios mergedCrossings
    mergedCrossings = foldl1 mergeRatios crossings
    crossings = map multiplyRatio [first, mixed, second]
    first = (firstGeneration fParent fParent, fRatio^2)
    mixed = (firstGeneration fParent sParent, 2 * (fRatio * sRatio))
    second = (firstGeneration sParent sParent, sRatio^2)
