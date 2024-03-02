-- cabal test --test-show-details=direct

import           Control.Exception (evaluate)
import           Convolve          (convolve2DSeparable, normalizeMatrix)
import           Data.Matrix       as M
import           Model
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

        it "makes an ant" $ do
            let gen = mkStdGen 0
            let (actualAnt, _) = mkAnt 1 2 2 gen
            let expectedAnt = Ant 1 3 3 North SeekFood 0 300
            actualAnt `shouldBe` expectedAnt

    describe "convolve2DSeparable" $ do
        it "convolves a 5x5 matrix with a 1x3 kernel and a 3x1 kernel" $ do
            let inputMatrix = M.fromLists [[1..5], [6..10], [11..15], [16..20], [21..25]] :: Matrix Float
                kernelX = M.fromList 1 3 [1, 2, 1] :: Matrix Float
                kernelY = M.fromList 3 1 [1, 2, 1] :: Matrix Float
                resultMatrix = convolve2DSeparable inputMatrix kernelX kernelY
            resultMatrix `shouldBe` M.fromLists [[27.0,44.0,56.0,68.0,57.0],[76.0,112.0,128.0,144.0,116.0],[136.0,192.0,208.0,224.0,176.0],[196.0,272.0,288.0,304.0,236.0],[177.0,244.0,256.0,268.0,207.0]]

        it "convolves a 5x5 matrix with a 1x5 kernel and a 5x1 kernel" $ do
            let inputMatrix = M.fromLists [[1..5], [6..10], [11..15], [16..20], [21..25]] :: Matrix Float
                kernelX = M.fromList 1 5 [1, 4, 6, 4, 1] :: Matrix Float
                kernelY = M.fromList 5 1 [1, 4, 6, 4, 1] :: Matrix Float
                resultMatrix = convolve2DSeparable inputMatrix kernelX kernelY
            resultMatrix `shouldBe` M.fromLists [[517.0,802.0,1008.0,1088.0,869.0],[1190.0,1755.0,2080.0,2145.0,1670.0],[2032.0,2912.0,3328.0,3328.0,2544.0],[2620.0,3705.0,4160.0,4095.0,3100.0],[2277.0,3202.0,3568.0,3488.0,2629.0]]

        it "normalizes a matrix" $ do
            let m = M.fromLists [[1, 2, 1], [2, 4, 2], [1, 2, 1]] :: Matrix Float
            let resultMatrix = normalizeMatrix m
            resultMatrix `shouldBe` M.fromLists [[6.25e-2,0.125,6.25e-2],[0.125,0.25,0.125],[6.25e-2,0.125,6.25e-2]]
