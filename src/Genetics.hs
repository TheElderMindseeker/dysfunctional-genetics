module Genetics
    (
      Dominance
    , Allele
    , Gene
    , Genotype
    , Ratio
    , firstGeneration
    , inferPhenotype
    ) where

-- | In genetics an allele can be either dominant or recessive.
-- Dominant alleles take upper hand over recessive in genes.
data Dominance = Dominant
               | Recessive deriving Show

-- | Allele is a combination of dominance and integer identifier.
-- Integer ID is used to distinguish between different types of alleles.
type Allele = (Dominance, Integer)

-- | Gene consists of two alleles.
type Gene = (Allele, Allele)

-- | Genotype is a list of genes.
-- Genes in a genotype must contain different alleles (with different IDs).
type Genotype = [Gene]

-- | Convenience type used in generation computing functions.
type Ratio = (Genotype, Integer)

-- | Helper function for calculating crossing ratios.
dominance :: Allele -> Integer
dominance (Dominant, _) = 1
dominance (Recessive, _) = 0

-- | Helper function for calculating crossing ratios.
geneDominance :: Gene -> Integer
geneDominance gene = dominance (fst gene) + dominance (snd gene)

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
    dominant = (dominantGene, fDom * sDom)
    mixed = (mixedGene, fDom * sRec + fRec * sDom)
    recessive = (recessiveGene, fRec * sRec)
    fDom = geneDominance fGene
    fRec = 2 - fDom
    sDom = geneDominance sGene
    sRec = 2 - sDom
    zeroRatio (_, ratio) = ratio /= 0
    alleleId = snd (fst fGene)
    dominantAllele = (Dominant, alleleId)
    recessiveAllele = (Recessive, alleleId)
    dominantGene = (dominantAllele, dominantAllele)
    mixedGene = (dominantAllele, recessiveAllele)
    recessiveGene = (recessiveAllele, recessiveAllele)

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

-- | Infer organism phenotype from its genotype and allele behaviour.
inferPhenotype :: Genotype           -- ^ Organism genotype
               -> (Allele -> trait)  -- ^ Allele behaviour mapping
               -> [trait]            -- ^ Organism phenotype
inferPhenotype _ _ = []
