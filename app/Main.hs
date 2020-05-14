module Main where

import           Genetics
import qualified Data.Set                      as Set
import           Data.Array
import           Data.Ratio
import           Debug.Trace
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort


-- | Gloss window to render the elements to
window :: Display
window = InWindow "Genetics Demo" (640, 480) (10, 10)

-- | Draws a Punnett square of a genetic crossover of 2 parents
punnettSquare 
    :: (Show allele, Ord allele) 
    => Genotype allele 
    -> Genotype allele 
    -> Picture
-- | Note: I do understand that we have a genetics library
-- | But it doesn't work for constructing true punnett squares
--
-- Not sure why but I can't replicate the automagic of pictures() and translate()
-- I tried reading the code, but don't think I found the right parts
-- It appears that pictures is evaluated at runtime at once,
--  so translations appear local with pictures(), but global with other funcs
punnettSquare p1 p2 = punnettSquare' (elems p1) (elems p2)

-- | Helper function to deal with local transforms
punnettSquare'
    :: (Show allele, Ord allele) 
    => [Gene allele] 
    -> [Gene allele] 
    -> Picture
punnettSquare' [] _ = blank
punnettSquare' (e1:p1) (e2:p2) = pictures [drawRow e2, 
  translate (-100) (-200) (drawColumn e1),
  translate (100) (-100) (drawSquare e1 e2),
  translate 500 0 (punnettSquare' p1 p2)]

-- | Draws a row from a gene
drawRow
    :: (Show allele, Ord allele) 
    => Gene allele
    -> Picture
drawRow gene = scale 0.5 0.5 (gene2 <> translate spacing 0 gene1)
  where
    geneList = Set.toList gene
    gene1 = Text (show (geneList !! 0))
    gene2 = Text (show (geneList !! 1))
    spacing = 200

-- | Draws a column from a Gene
drawColumn
    :: (Show allele, Ord allele) 
    => Gene allele
    -> Picture
drawColumn gene = scale 0.5 0.5 (gene2 <> translate 0 spacing gene1)
  where
    geneList = Set.toList gene
    gene1 = Text (show (geneList !! 0))
    gene2 = Text (show (geneList !! 1))
    spacing = 200

-- | Draws the inner part of the PunnetSquare with the results of the 2 crossed genes 
drawSquare
    :: (Show allele, Ord allele) 
    => Gene allele
    -> Gene allele
    -> Picture
drawSquare p1 p2 = scale 0.5 0.5 (pictures[Text (show (g1 !! 0)),
  translate (-200) 0 $ Text (show (g1 !! 1)),
  translate 0 (-200) $ Text (show (g2 !! 0)),
  translate (-200) (-200) $ Text (show (g2 !! 1))])
  where
    g1list = zip (replicate 2 ((Set.toList p1) !! 0)) (Set.toList p2)
    g2list = zip (replicate 2 ((Set.toList p1) !! 1)) (Set.toList p2)
    g1 = map pairToList g1list
    g2 = map pairToList g2list

-- | Convert a tuple pair of values into an array
pairToList
    :: (a, a)
    -> [a]
pairToList (x, y) = [x, y]

-- | Draws a genetics branch diagrams
branchDiagram
    :: (Show allele,Ord allele)
    => Genotype allele 
    -> Genotype allele 
    -> Picture
branchDiagram p1 p2 = scale 0.5 0.5 (branchDiagram' (elems geneRatio) (zip (elems p1) (elems p2)))
  where 
    geneRatio = crossGenotypes p1 p2

-- | Draws all of the diagrams for a crossover (all parent genes + results)
branchDiagram'
    :: (Show allele,Ord allele)
    => [[GeneRatio allele]]
    -> [(Gene allele, Gene allele)]
    -> Picture
branchDiagram' [] _ = blank
branchDiagram' (ratio:ratios) (parents:allAlleles) = pictures [drawDiagram ratio parents, 
  translate spacing 0 $ branchDiagram' ratios allAlleles]
  where
    spacing = 5000

-- | Draws the diagram for a single gene group (single gene pair + results)
-- | This particular function appends the parent alleles
drawDiagram
    :: (Show allele,Ord allele)
    => [GeneRatio allele]
    -> (Gene allele, Gene allele)
    -> Picture
drawDiagram ratios parents = translate 0 yShift (parentPictures) <> drawDiagram' ratios
  where
    yShift = 1000
    spacing = 500
    p1 = Text (show (Set.toList (fst parents)))
    p2 = Text (show (Set.toList (snd parents)))
    parentPictures = pictures [p1, translate spacing 0 p2]

-- | This function renders a line of alllels
drawDiagram'
    :: (Show allele,Ord allele)
    => [GeneRatio allele]
    -> Picture
drawDiagram' [] = blank
drawDiagram' (ratio:ratios) = pictures [drawGene ratio,
  translate spacing 0 $ drawDiagram' ratios]
  where
    spacing = 800

-- | Draws a single GeneRatio, by showing the genes above and ratio below
drawGene
    :: (Show allele, Ord allele)
    => GeneRatio allele
    -> Picture
drawGene ratio = pictures [translate xShift yShift allele, probability]
  where
    allele = Text (show (Set.toList (fst ratio)))
    probability = Text (show (snd ratio))
    yShift = 150
    xShift = 100

-- | Parent genomes definition
parent1 :: Genotype Char
parent1 = listArray (0, 0) [gene_aa]
  where
    gene_aa = Set.fromList "aA"

parent2 :: Genotype Char
parent2 = listArray (0, 0) [gene_bb]
  where
    gene_bb = Set.fromList "bB"

parent3 :: Genotype Char
parent3 = listArray (0, 1) [gene_a, gene_A]
  where
    gene_a = Set.fromList "aA"
    gene_A = Set.fromList "bB"

parent4 :: Genotype Char
parent4 = listArray (0, 1) [gene_b, gene_B]
  where
    gene_b = Set.fromList "cC"
    gene_B = Set.fromList "dD"

parent5 :: Genotype Char
parent5 = listArray (0, 0) [gene_aa]
  where
    gene_aa = Set.fromList "aa"
    
parent6 :: Genotype Char
parent6 = listArray (0, 0) [gene_bb]
  where
    gene_bb = Set.fromList "bb"

-- | Main function
main :: IO ()
main = display window white ((punnettSquare parent3 parent4) 
    <> (translate 0 (-1000) (branchDiagram parent3 parent4)))
