-- cabal test ant-ai

module Main where

import Data.Function ((&))
import Data.Vector (Vector)
import Data.Vector qualified as V
import NeuralNetwork
import System.Random (mkStdGen)
import System.Directory (removeDirectoryRecursive)
import Test.Hspec
import Text.Printf (printf)


-- Test with known XOR-NXOR model weights and biases from PyTorch
testHiddenWeights :: Vector (Vector Float)
testHiddenWeights =
    V.fromList
        [ V.fromList [-5.0524, -5.2451],
          V.fromList [2.7441, 3.0019],
          V.fromList [-7.6151, -7.4653],
          V.fromList [-2.0832, 0.7891]
        ]


testHiddenBiases :: Vector Float
testHiddenBiases = V.fromList [7.8758, -4.5018, 3.3328, -0.6623]


testOutputWeights :: Vector (Vector Float)
testOutputWeights =
    V.fromList
        [ V.fromList [7.5012, -4.3334, -10.7212, 0.9675],
          V.fromList [-7.5469, 4.2851, 10.7247, -0.9566]
        ]


testOutputBiases :: Vector Float
testOutputBiases = V.fromList [-1.8809, 1.9244]


-- Test inputs
testInputs :: Vector (Vector Float)
testInputs =
    V.fromList
        [ V.fromList [0.0, 0.0],
          V.fromList [0.0, 1.0],
          V.fromList [1.0, 0.0],
          V.fromList [1.0, 1.0]
        ]


testPredictions :: [[String]]
testPredictions =
    [ ["0.0115", "0.9885"],
      ["0.9907", "0.0093"],
      ["0.9887", "0.0112"],
      ["0.0108", "0.9892"]
    ]


layers :: [Layer]
layers = [(testHiddenWeights, testHiddenBiases), (testOutputWeights, testOutputBiases)]


main :: IO ()
main = hspec $ do
    describe "XorNxorForward" $ do
        it "forwardAll" $ do
            let predictions = testInputs & fmap (forwardAll sigmoid layers) & vecOfVecsToListOfLists
                predictionsRounded = map (map (printf "%.4f" :: Float -> String)) predictions
            predictionsRounded `shouldBe` testPredictions

        it "flatten/unflatten isomorphism" $ do
            let flatLayers = flattenLayers layers
            unflattenLayers (flattenLayers layers) `shouldBe` layers
            flattenLayers (unflattenLayers flatLayers) `shouldBe` flatLayers

        it "initFlatLayers" $ do
            let ((initFlat, initShapes), rng') = initFlatLayers [2, 4, 2] 0.1 (mkStdGen 0)
                (flatAll, shapesAll) = flattenLayers layers
            length initFlat `shouldBe` length flatAll
            initShapes `shouldBe` shapesAll

        it "crossover" $ do
            let ((parent1, shapes1), rng') = initFlatLayers [2, 4, 2] 0.1 (mkStdGen 0)
                ((parent2, shapes2), rng'') = initFlatLayers [2, 4, 2] 0.1 rng'
                ((child, shapes3), rng''') = crossover (parent1, shapes1) (parent2, shapes2) rng''
                prnt1Rounded = map (printf "%.4f" :: Float -> String) parent1
                prnt2Rounded = map (printf "%.4f" :: Float -> String) parent2
                childRounded = map (printf "%.4f" :: Float -> String) child
            prnt1Rounded `shouldBe` ["0.0947", "-0.0176", "-0.0905", "-0.0933", "-0.0409", "-0.0119", "-0.0739", "-0.0751", "-0.0807", "0.0261", "-0.0190", "0.0762", "0.0526", "-0.0450", "0.0133", "0.0597", "0.0380", "0.0997", "-0.0268", "0.0711", "0.0450", "-0.0224"]
            prnt2Rounded `shouldBe` ["0.0936", "-0.0727", "-0.0302", "-0.0867", "0.0455", "-0.0558", "0.0634", "-0.0709", "-0.0265", "0.0716", "-0.0047", "0.0803", "0.0329", "0.0948", "0.0167", "0.0638", "-0.0427", "-0.0517", "0.0174", "0.0802", "-0.0410", "0.0742"]
            childRounded `shouldBe` ["0.0947", "-0.0176", "-0.0905", "-0.0933", "0.0455", "-0.0119", "-0.0739", "-0.0751", "-0.0265", "0.0261", "-0.0190", "0.0762", "0.0329", "-0.0450", "0.0167", "0.0638", "-0.0427", "0.0997", "-0.0268", "0.0802", "0.0450", "0.0742"]
            shapes3 `shouldBe` [(4, 2), (2, 4)]

        it "mutate" $ do
            let ((orig, shapes1), rng') = initFlatLayers [2, 4, 2] 0.1 (mkStdGen 0)
                ((mutated, shapes2), rng'') = mutate 0.1 0.1 ((orig, shapes1), rng')
                origRounded = map (printf "%.4f" :: Float -> String) orig
                mutaRounded = map (printf "%.4f" :: Float -> String) mutated
            origRounded `shouldBe` ["0.0947", "-0.0176", "-0.0905", "-0.0933", "-0.0409", "-0.0119", "-0.0739", "-0.0751", "-0.0807", "0.0261", "-0.0190", "0.0762", "0.0526", "-0.0450", "0.0133", "0.0597", "0.0380", "0.0997", "-0.0268", "0.0711", "0.0450", "-0.0224"]
            mutaRounded `shouldBe` ["0.0947", "-0.0176", "-0.0905", "-0.1467", "-0.0409", "-0.0119", "-0.0739", "-0.0751", "-0.0807", "0.0261", "-0.0190", "0.0762", "0.0526", "-0.0450", "0.0133", "0.0597", "0.0380", "0.0997", "-0.0268", "0.0711", "0.0450", "-0.0224"]

        it "write/read" $ do
            let ((flat, shapes), _) = initFlatLayers [2, 4, 2] 0.1 (mkStdGen 0)
                filepath = "testdata/flatLayers.bin"
            writeFlatLayers filepath (flat, shapes)
            (flat', shapes') <- readFlatLayers filepath
            (flat', shapes') `shouldBe` (flat, shapes)
            removeDirectoryRecursive "testdata"