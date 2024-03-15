-- cabal test --test-show-details=direct

import           AntSim
import           Control.Exception (evaluate)
import           Convolve          (convolve1D, convolve2DSeparable,
                                    normalizeList, normalizeMatrix)
import           Data.Matrix       as M
import           System.Random
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Prelude.head" $ do
        it "returns the first element of a list" $ do
            head [23 ..] `shouldBe` (23 :: Int)

        it "returns the first element of an *arbitrary* list" $ do
            property $ \x xs -> head (x:xs) == (x :: Int)

        it "throws an exception if used with an empty list" $ do
            evaluate (head []) `shouldThrow` anyException

    describe "Convolve" $ do
        it "convolves a 5x5 matrix with a 1x3 kernel and a 3x1 kernel" $ do
            let inputMatrix = M.fromLists [[1..5], [6..10], [11..15], [16..20], [21..25]] :: Matrix Float
                kernelX = [1, 2, 1]
                kernelY = [1, 2, 1]
                resultMatrix = convolve2DSeparable inputMatrix kernelX kernelY
            resultMatrix `shouldBe` M.fromLists [[27.0,44.0,56.0,68.0,57.0],[76.0,112.0,128.0,144.0,116.0],[136.0,192.0,208.0,224.0,176.0],[196.0,272.0,288.0,304.0,236.0],[177.0,244.0,256.0,268.0,207.0]]

        it "convolves a 5x5 matrix with a 1x5 kernel and a 5x1 kernel" $ do
            let inputMatrix = M.fromLists [[1..5], [6..10], [11..15], [16..20], [21..25]] :: Matrix Float
                kernelX = [1, 4, 6, 4, 1]
                kernelY = [1, 4, 6, 4, 1]
                resultMatrix = convolve2DSeparable inputMatrix kernelX kernelY
            resultMatrix `shouldBe` M.fromLists [[517.0,802.0,1008.0,1088.0,869.0],[1190.0,1755.0,2080.0,2145.0,1670.0],[2032.0,2912.0,3328.0,3328.0,2544.0],[2620.0,3705.0,4160.0,4095.0,3100.0],[2277.0,3202.0,3568.0,3488.0,2629.0]]

        it "normalizes a matrix" $ do
            let m = M.fromLists [[1, 2, 1], [2, 4, 2], [1, 2, 1]] :: Matrix Float
            let resultMatrix = normalizeMatrix m
            resultMatrix `shouldBe` M.fromLists [[0.0625,0.125,0.0625],[0.125,0.25,0.125],[0.0625,0.125,0.0625]]

        it "proves that normalizing the separable kernels and then combining them equals the normalized combined kernel" $ do
            let kernelX = normalizeMatrix (M.fromList 1 3 [1, 2, 1] :: Matrix Float)
                kernelY = normalizeMatrix (M.fromList 3 1 [1, 2, 1] :: Matrix Float)
                combinedKernel1 = M.multStd2 kernelY kernelX
                combinedKernel2 = normalizeMatrix( M.fromLists [[1, 2, 1], [2, 4, 2], [1, 2, 1]] :: Matrix Float)
            combinedKernel1 `shouldBe` combinedKernel2

        it "normalizes a list" $ do
            normalizeList [1, 2, 1] `shouldBe` ([0.25, 0.5, 0.25] :: [Float])
            normalizeList [1, 4, 6, 4, 1] `shouldBe` ([0.0625,0.25,0.375,0.25,0.0625] :: [Float])


        it "convolves two 1D lists and gives the same result as numpy convolve" $ do
            convolve1D [0..9] [0..2] `shouldBe` ([0, 1, 4, 7, 10, 13, 16, 19, 22, 25] :: [Int])
            convolve1D [0..2] [0..9] `shouldBe` ([0, 1, 4, 7, 10, 13, 16, 19, 22, 25] :: [Int])
            convolve1D [0..9] [0..3] `shouldBe` ([0, 1, 4, 10, 16, 22, 28, 34, 40, 46] :: [Int])
            convolve1D [0..3] [0..9] `shouldBe` ([0, 1, 4, 10, 16, 22, 28, 34, 40, 46] :: [Int])
            convolve1D [0..9] [0..4] `shouldBe` ([1, 4, 10, 20, 30, 40, 50, 60, 70, 70] :: [Int])
            convolve1D [0..4] [0..9] `shouldBe` ([1, 4, 10, 20, 30, 40, 50, 60, 70, 70] :: [Int])
            convolve1D [-5..4] [0..2] `shouldBe` ([-5, -14, -11, -8, -5, -2, 1, 4, 7, 10] :: [Int])
            convolve1D [0..2] [-5..4] `shouldBe` ([-5, -14, -11, -8, -5, -2, 1, 4, 7, 10] :: [Int])
            convolve1D [0..8] [0..4] `shouldBe` ([1, 4, 10, 20, 30, 40, 50, 60, 61] :: [Int])
            convolve1D [0..4] [0..8] `shouldBe` ([1, 4, 10, 20, 30, 40, 50, 60, 61] :: [Int])
