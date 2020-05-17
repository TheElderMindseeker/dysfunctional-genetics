# Genetics Library

This library implements functions for genetic crossing and inferring phenotypes from genotypes. It can help you in your biology / genetics courses or be used for implementing genetics algorithms with realistic crossing strategies.

# Basic Usage

The library exports some basic types for creating genes, genotypes, and their ratios. A gene is just a set of alleles (variations of the trait) and should contain either one or two elements for real-world tasks. Genotype is an array
of genes.

When using the library you may want to define organisms' traits by applying `simpleTrait`, `incDomTrait`, and `multiTrait` or even your own trait functions. Then, you should construct genotypes of parent organisms using any array constructor you find suitable or invoking `genotype` function provided by our library.

The function `crossGenotypes` can be used to cross genotypes of two parents and produce genotype ratios of their children. The function returns the result in a packed form, use `unpack` to get result more suitable for working with.

After getting desired genotypes, their corresponding organisms' traits can be inferred using `phenotype` function.

# Usage Example

```haskell
gtype      = genotype ["Ls", "Wb"]
tailTrait  = simpleTrait 0 'L' "long" "short"
colorTrait = incDomTrait 1 'W' "white" "white & black" "black"
traits     = [tailTrait, colorTrait]
phenotype gtype traits  -- should be ["long", "white & black"]
```
