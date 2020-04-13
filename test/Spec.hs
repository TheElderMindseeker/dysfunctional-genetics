import Test.Hspec
import Data.Array
import qualified Data.Set as Set
import Genetics

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

    describe "Phenotype tests" $ do
        let genotype = listArray (0, 1) [Set.fromList "Ls", Set.fromList "Wb"]
        let tailTrait = simpleTrait 0 'L' "long" "short"
        let colorTrait = incDomTrait 1 'W' "white" "white & black" "black"
        let traits = [tailTrait, colorTrait]

        it "Calculate phenotype from genotype" $
            phenotype genotype traits `shouldBe` ["long", "white & black"]

