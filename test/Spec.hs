import Test.Hspec
import Data.Array
import qualified Data.Set as Set
import Genetics

toColor :: Char -> String
toColor symbol
    | symbol == 'r' = "red"
    | symbol == 'o' = "orange"
    | symbol == 'y' = "yellow"
    | symbol == 'g' = "green"
    | symbol == 'c' = "cyan"
    | symbol == 'b' = "blue"
    | symbol == 'v' = "violet"
    | otherwise = "White"

main :: IO ()
main = hspec $ do
    describe "Simple trait tests" $ do
        let longShortTrait = simpleTrait 0 'L' "long" "short"

        it "Dominant trait from dominant gene" $
            let genotype = listArray (0, 0) [Set.fromList "LL"] in
                longShortTrait genotype `shouldBe` "long"

        it "Dominant trait from mixed gene" $
            let genotype = listArray (0, 0) [Set.fromList "Ls"] in
                longShortTrait genotype `shouldBe` "long"

        it "Recessive trait from recessive gene" $
            let genotype = listArray (0, 0) [Set.fromList "ss"] in
                longShortTrait genotype `shouldBe` "short"

    describe "Incomplete dominance tests" $ do
        let colorTrait = incDomTrait 0 'W' "white" "grey" "black"

        it "Dominant trait from dominant gene" $
            let genotype = listArray (0, 0) [Set.fromList "WW"] in
                colorTrait genotype `shouldBe` "white"

        it "Mixed trait from mixed gene" $
            let genotype = listArray (0, 0) [Set.fromList "Wb"] in
                colorTrait genotype `shouldBe` "grey"

        it "Recessive trait from recessive gene" $
            let genotype = listArray (0, 0) [Set.fromList "bb"] in
                colorTrait genotype `shouldBe` "black"

    describe "Multiple allele tests" $ do
        let colorTrait = multiTrait 0 ['r', 'o', 'y', 'g', 'c', 'b', 'v'] toColor

        it "Most dominant trait" $
            let genotype = listArray (0, 0) [Set.fromList "rg"] in
                colorTrait genotype `shouldBe` "red"

        it "More dominant trait" $
            let genotype = listArray (0, 0) [Set.fromList "yc"] in
                colorTrait genotype `shouldBe` "yellow"

        it "Trait from one-allele gene" $
            let genotype = listArray (0, 0) [Set.fromList "bb"] in
                colorTrait genotype `shouldBe` "blue"

    describe "Phenotype tests" $ do
        let genotype = listArray (0, 1) [Set.fromList "Ls", Set.fromList "Wb"]
        let tailTrait = simpleTrait 0 'L' "long" "short"
        let colorTrait = incDomTrait 1 'W' "white" "white & black" "black"
        let traits = [tailTrait, colorTrait]

        it "Calculate phenotype from genotype" $
            phenotype genotype traits `shouldBe` ["long", "white & black"]

