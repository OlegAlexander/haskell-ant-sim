-- This module was written with help from ChagGPT.

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Convolve where

import           Data.Matrix (Matrix)
import qualified Data.Matrix as M
import           Data.Maybe  (catMaybes, fromMaybe)


convolve2DSeparable :: Num a => Matrix a -> Matrix a -> Matrix a -> Matrix a
convolve2DSeparable input kernelX kernelY = convolveVertical (convolveHorizontal input kernelX) kernelY

convolveHorizontal :: Num a => Matrix a -> Matrix a -> Matrix a
convolveHorizontal input kernelX =
  M.matrix (M.nrows input) (M.ncols input) $ \(i, j) ->
    let kCenterX = (M.ncols kernelX `div` 2) + 1
        indicesX = [1 .. M.ncols kernelX]
        values = [ fromMaybe 0 (M.safeGet i (j + k - kCenterX) input) * fromMaybe 0 (M.safeGet 1 k kernelX)
                 | k <- indicesX
                 , let offset = k - kCenterX
                 , j + offset > 0 && j + offset <= M.ncols input
                 ]
    in sum values

convolveVertical :: Num a => Matrix a -> Matrix a -> Matrix a
convolveVertical input kernelY =
  M.matrix (M.nrows input) (M.ncols input) $ \(i, j) ->
    let kCenterY = (M.nrows kernelY `div` 2) + 1
        indicesY = [1 .. M.nrows kernelY]
        values = [ fromMaybe 0 (M.safeGet (i + k - kCenterY) j input) * fromMaybe 0 (M.safeGet k 1 kernelY)
                 | k <- indicesY
                 , let offset = k - kCenterY
                 , i + offset > 0 && i + offset <= M.nrows input
                 ]
    in sum values

normalizeMatrix :: Fractional a => Matrix a -> Matrix a
normalizeMatrix m = M.scaleMatrix (1 / sum (M.toList m)) m


test3x3 :: IO ()
test3x3 = do
  let inputMatrix = M.fromLists [[1..5], [6..10], [11..15], [16..20], [21..25]] :: Matrix Float
      kernelX = M.fromList 1 3 [1, 2, 1] :: Matrix Float
      kernelY = M.fromList 3 1 [1, 2, 1] :: Matrix Float
      resultMatrix = convolve2DSeparable inputMatrix kernelX kernelY
  print (M.toLists resultMatrix)

test5x5 :: IO ()
test5x5 = do
  let inputMatrix = M.fromLists [[1..5], [6..10], [11..15], [16..20], [21..25]] :: Matrix Float
      kernelX = M.fromList 1 5 [1, 4, 6, 4, 1] :: Matrix Float
      kernelY = M.fromList 5 1 [1, 4, 6, 4, 1] :: Matrix Float
      resultMatrix = convolve2DSeparable inputMatrix kernelX kernelY
  print (M.toLists resultMatrix)


testNormalize :: IO ()
testNormalize = do
    let m = M.fromLists [[1, 2, 1], [2, 4, 2], [1, 2, 1]] :: Matrix Float
    print (M.toLists (normalizeMatrix m))

-- image
-- array([[ 1,  2,  3,  4,  5],
--        [ 6,  7,  8,  9, 10],
--        [11, 12, 13, 14, 15],
--        [16, 17, 18, 19, 20],
--        [21, 22, 23, 24, 25]])

-- 3x3
-- >>> kernelX = np.array([[1,2,1]])
-- >>> kernelX
-- array([[1, 2, 1]])

-- >>> kernelY = np.array([[1],[2],[1]])
-- >>> kernelY
-- array([[1],
--        [2],
--        [1]])

-- >>> kernel = np.dot(kernelY, kernelX)
-- >>> kernel
-- array([[1, 2, 1],
--        [2, 4, 2],
--        [1, 2, 1]])

-- >>> res = convolve2d(image, kernel, mode='same', boundary='fill', fillvalue=0)
-- >>> res
-- array([[ 27,  44,  56,  68,  57],
--        [ 76, 112, 128, 144, 116],
--        [136, 192, 208, 224, 176],
--        [196, 272, 288, 304, 236],
--        [177, 244, 256, 268, 207]])

-- 5x5

-- >>> kernelX = np.array([[1,4,6,4,1]])
-- >>> kernelY = np.array([[1],[4],[6],[4],[1]])
-- >>> kernel = np.dot(kernelY, kernelX)
-- >>> kernel
-- array([[ 1,  4,  6,  4,  1],
--        [ 4, 16, 24, 16,  4],
--        [ 6, 24, 36, 24,  6],
--        [ 4, 16, 24, 16,  4],
--        [ 1,  4,  6,  4,  1]])
-- >>> res = convolve2d(image, kernel, mode='same', boundary='fill', fillvalue=0)
-- >>> res
-- array([[ 517,  802, 1008, 1088,  869],
--        [1190, 1755, 2080, 2145, 1670],
--        [2032, 2912, 3328, 3328, 2544],
--        [2620, 3705, 4160, 4095, 3100],
--        [2277, 3202, 3568, 3488, 2629]])
