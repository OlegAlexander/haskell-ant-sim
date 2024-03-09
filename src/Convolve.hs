{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Convolve where

import           Data.Matrix (Matrix)
import qualified Data.Matrix as M

convolve1D :: Num a => [a] -> [a] -> [a]
convolve1D x w
    | length x < length w = convolve1D w x -- Ensure that x is the longer list
    | otherwise =
        let (lx, lw) = (length x, length w)
            lPad = lw `div` 2; rPad = lw - lPad
            xPadded = replicate lPad 0 ++ x ++ replicate rPad 0
            wR = reverse w
            convolveAt i = sum $ zipWith (*) (take lw (drop i xPadded)) wR
        in map convolveAt [0..lx - 1]


convolve2DSeparable :: Num a => Matrix a -> [a] -> [a] -> Matrix a
convolve2DSeparable m kx ky = convolveV (convolveH m kx) ky
    where
        convolveH m' kx' = M.fromLists $ map (convolve1D kx') (M.toLists m')
        convolveV m' ky' =
            let mT = M.transpose m'
            in M.transpose $ M.fromLists $ map (convolve1D ky') (M.toLists mT)


normalizeMatrix :: Fractional a => Matrix a -> Matrix a
normalizeMatrix m = M.scaleMatrix (1 / sum (M.toList m)) m


normalizeList :: Fractional a => [a] -> [a]
normalizeList l = let sum' = sum l in map (/ sum') l


testConvolve1D :: IO ()
testConvolve1D = do
  -- Expected results based on numpy's convolve function in 'same' mode
  print $ convolve1D [0..9] [0..2]  -- [0, 1, 4, 7, 10, 13, 16, 19, 22, 25]
  print $ convolve1D [0..2] [0..9]  -- [0, 1, 4, 7, 10, 13, 16, 19, 22, 25]
  print $ convolve1D [0..9] [0..3]  -- [0, 1, 4, 10, 16, 22, 28, 34, 40, 46]
  print $ convolve1D [0..3] [0..9]  -- [0, 1, 4, 10, 16, 22, 28, 34, 40, 46]
  print $ convolve1D [0..9] [0..4]  -- [1, 4, 10, 20, 30, 40, 50, 60, 70, 70]
  print $ convolve1D [0..4] [0..9]  -- [1, 4, 10, 20, 30, 40, 50, 60, 70, 70]
  print $ convolve1D [-5..4] [0..2] -- [-5, -14, -11, -8, -5, -2, 1, 4, 7, 10]
  print $ convolve1D [0..2] [-5..4] -- [-5, -14, -11, -8, -5, -2, 1, 4, 7, 10]
  print $ convolve1D [0..8] [0..4] --  [ 1,  4, 10, 20, 30, 40, 50, 60, 61]
  print $ convolve1D [0..4] [0..8] --  [ 1,  4, 10, 20, 30, 40, 50, 60, 61]


test3x3 :: IO ()
test3x3 = do
  let inputMatrix = M.fromLists [[1..5], [6..10], [11..15], [16..20], [21..25]] :: Matrix Float
      kernelX = [1, 2, 1]
      kernelY = [1, 2, 1]
      resultMatrix = convolve2DSeparable inputMatrix kernelX kernelY
  print resultMatrix


test5x5 :: IO ()
test5x5 = do
  let inputMatrix = M.fromLists [[1..5], [6..10], [11..15], [16..20], [21..25]] :: Matrix Float
      kernelX = [1, 4, 6, 4, 1]
      kernelY = [1, 4, 6, 4, 1]
      resultMatrix = convolve2DSeparable inputMatrix kernelX kernelY
  print resultMatrix


testNormalizeMatrix :: IO ()
testNormalizeMatrix = do
    let m = M.fromLists [[1, 2, 1], [2, 4, 2], [1, 2, 1]] :: Matrix Float
    print (M.toLists (normalizeMatrix m))


testNormalizeMatrix2 :: IO ()
testNormalizeMatrix2 = do
    let kernelX = normalizeMatrix(M.fromList 1 3 [1, 2, 1] :: Matrix Float)
        kernelY = normalizeMatrix(M.fromList 3 1 [1, 2, 1] :: Matrix Float)
        resultMatrix = M.multStd2 kernelY kernelX
    print resultMatrix


testNormalizeList :: IO ()
testNormalizeList = do
    print $ normalizeList [1, 2, 1]
    print $ normalizeList [1, 4, 6, 4, 1]
