module Genetics
    (
      Dominance
    , Allele
    , Gene
    , Genotype
    , Trait
    , Phenotype
    , Ratio
    , firstGeneration
    , inferPhenotype
    ) where

-- | In genetics an allele can be either dominant or recessive.
-- Dominant alleles take upper hand over recessive in genes.
data Dominance = Dominant
               | Recessive

-- | Allele is a combination of dominance and integer identifier.
-- Integer ID is used to distinguish between different types of alleles.
type Allele = (Dominance, Integer)

-- | Gene consists of two alleles.
type Gene = (Allele, Allele)

-- | Genotype is a list of genes.
-- Genes in a genotype must contain different alleles (with different IDs).
type Genotype = [Gene]

-- | Trait is just a desription of the feature of a organism.
type Trait = String

-- | Phenotype is a list of traits.
type Phenotype = [Trait]

-- | Convenience type used in generation computing functions.
type Ratio = (Genotype, Integer)

-- | Calculates the ratios of genotypes in the first generation.
firstGeneration :: Genotype  -- ^ Genotype of the first parent
                -> Genotype  -- ^ Genotype of the second parent
                -> [Ratio]   -- ^ List of children genotypes with ratios

-- | Infer organism phenotype from its genotype and allele behaviour.
inferPhenotype :: Genotype           -- ^ Organism genotype
               -> (Allele -> Trait)  -- ^ Allele behaviour mapping
               -> Phenotype          -- ^ Organism phenotype
