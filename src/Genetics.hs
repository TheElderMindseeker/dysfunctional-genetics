module Genetics
    (
      Gene
    , GeneRatio
    , Genotype
    , GenotypeRatio
    , crossGenes
    , crossGenotypes
    , simpleTrait
    , incDomTrait
    , multiTrait
    , phenotype
    ) where

import Data.Array
import Data.Ratio
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | A type representing a single gene.
-- In genetics, each organism holds two copies of each gene in its genotype.
-- The gene value is called allele and each gene can have two or more possible
-- alleles. The sequence of gene copies is not important, so mixed genes such
-- as (Aa) are the same as (aA), and are, therefore, represented by the set.
type Gene allele = Set.Set allele

type GeneRatio allele = (Gene allele, Ratio Int)

-- | Genotype is array of genes.
type Genotype allele = Array Int (Gene allele)

-- | Genotype with gene ratios.
type GenotypeRatio allele = Array Int [GeneRatio allele]

crossGenes :: Ord allele
              => Gene allele
              -> Gene allele
              -> [(Gene allele, Ratio Int)]
crossGenes aGene bGene = Map.assocs geneRatios
  where
    geneRatios = foldl addGene Map.empty combinations
    addGene geneMap gene = Map.insertWith (+) gene (1 % norm) geneMap
    norm = length combinations
    combinations = [Set.fromList [x, y]
                    | x <- Set.toList aGene
                    , y <- Set.toList bGene]

crossGenotypes :: Ord allele
                  => Genotype allele       -- ^ First genotype
                  -> Genotype allele       -- ^ Second genotype
                  -> GenotypeRatio allele  -- ^ Children genotype ratios
crossGenotypes aGtype bGtype = listArray (0, resultLen) crossedGenes
  where
    resultLen = min (snd (bounds aGtype)) (snd (bounds bGtype))
    crossedGenes = zipWith (curry wCrossGenes) aList bList
    wCrossGenes (aGene, bGene) = crossGenes aGene bGene
    aList = elems aGtype
    bList = elems bGtype

-- | Simple trait has two possible allele values with strict dominance.
simpleTrait :: Ord allele
               => Int              -- ^ Gene index in genotype
               -> allele           -- ^ Dominant allele
               -> any              -- ^ Dominant trait
               -> any              -- ^ Recessive trait
               -> Genotype allele  -- ^ Genotype
               -> any              -- ^ Phenotype trait
simpleTrait index dominant domValue recValue genotype = traitValue
  where
    traitValue = if Set.member dominant (genotype ! index)
                 then domValue
                 else recValue

-- | Incomplete dominance traits may have a median state.
-- You can also express codominant traits with these if median trait is just
-- the combination of dominant and recessive.
-- Example: incDomTrait 0 'W' "white" "white & black" "black"
incDomTrait :: Ord allele
               => Int              -- ^ Gene index in genotype
               -> allele           -- ^ Dominant allele
               -> any              -- ^ Dominant trait
               -> any              -- ^ Median trait
               -> any              -- ^ Recessive trait
               -> Genotype allele  -- ^ Genotype
               -> any              -- ^ Phenotype trait
incDomTrait index dominant domValue medValue recValue genotype
    | Set.member dominant gene && Set.size gene == 1 = domValue
    | Set.size gene == 2 = medValue
    | otherwise = recValue
    where
        gene = genotype ! index

-- | Multiple alleles traits may have more than two alleles variants.
-- Though a single genotype can hold only up to two alleles, multiple allele
-- variants may be present in the population.
multiTrait :: Ord allele
              => Int              -- ^ Gene index in genotype
              -> [allele]         -- ^ Allele dominance order
              -> (allele -> any)  -- ^ Allele to trait correspondance
              -> Genotype allele  -- ^ Genotype
              -> any              -- ^ Phenotype trait
multiTrait index domOrder toTrait genotype = toTrait domAllele
  where
    domAllele = head (filter inGene domOrder)
    inGene allele = Set.member allele (genotype ! index)

-- | Infers the organism phenotype from its genotype.
phenotype :: Ord allele
             => Genotype allele           -- ^ Organism genotype
             -> [Genotype allele -> any]  -- ^ Traits
             -> [any]                     -- ^ Organism phenotype
phenotype genotype = map (\t -> t genotype)
