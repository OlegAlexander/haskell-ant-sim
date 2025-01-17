-- cabal test ant-ai

module Main where

import Data.Function ((&))
import Data.Vector (Vector)
import Data.Vector qualified as V
import NeuralNetwork
import System.Random (mkStdGen)
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

        it "unflattenLayers (flattenLayers layers) == layers" $ do
            unflattenLayers (flattenLayers layers) `shouldBe` layers

        it "initFlatLayers" $ do
            let ((initFlat, initShapes), rng') = initFlatLayers [2, 4, 2] 0.1 (mkStdGen 0)
                (flatAll, shapesAll) = flattenLayers layers
            length initFlat `shouldBe` length flatAll
            initShapes `shouldBe` shapesAll