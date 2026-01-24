{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Avoid lambda" #-}
module NeuralNetwork where

import Data.Function ((&))
import Data.List (foldl')
import Data.Tuple (swap)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Trace (traceShowId)
import System.Random (Random, StdGen, randomR)
import           Data.Serialize        (decode, encode)
import qualified Data.ByteString       as BS  
import           System.Directory      (createDirectoryIfMissing)
import           System.FilePath       (takeDirectory)


type FlatLayers = ([Float], [(Int, Int)])


type Layer = (Vector (Vector Float), Vector Float)


-- Dot product of two vectors
dotProd :: Vector Float -> Vector Float -> Float
dotProd xs ys = V.sum (V.zipWith (*) xs ys)


-- Forward pass for a single layer
-- forwardLayer :: (Float -> Float) -> Vector Float -> Layer -> Vector Float
-- forwardLayer activationFunc inputs (!weights, !biases) =
--     weights
--         & V.map (\ws -> dotProd inputs ws)
--         & V.zipWith (+) biases
--         & V.map activationFunc


-- 1. Expand (&) reverse application
-- Law: x & f = f x
-- forwardLayer :: (Float -> Float) -> Vector Float -> Layer -> Vector Float
-- forwardLayer activationFunc inputs (!weights, !biases) =
--     V.map activationFunc
--         (V.zipWith (+) biases
--             (V.map (\ws -> dotProd inputs ws) weights))


-- 2. Fusion of zipWith with map on its second argument
-- Law: zipWith f xs (map g ys) = zipWith (\x y -> f x (g y)) xs ys
-- forwardLayer :: (Float -> Float) -> Vector Float -> Layer -> Vector Float
-- forwardLayer activationFunc inputs (!weights, !biases) =
--     V.map activationFunc
--         (V.zipWith (\b ws -> b + dotProd inputs ws) biases weights)


-- 3. Swap zipWith arguments for readability
-- Law: zipWith f xs ys = zipWith (\y x -> f x y) ys xs
-- forwardLayer :: (Float -> Float) -> Vector Float -> Layer -> Vector Float
-- forwardLayer activationFunc inputs (!weights, !biases) =
--     V.map activationFunc
--         (V.zipWith (\ws b -> b + dotProd inputs ws) weights biases)


-- 4. Put b on the right for readability
-- Law: a + b = b + a
-- forwardLayer :: (Float -> Float) -> Vector Float -> Layer -> Vector Float
-- forwardLayer activationFunc inputs (!weights, !biases) =
--     V.map activationFunc
--         (V.zipWith (\ws b -> dotProd inputs ws + b) weights biases)


-- 5. Fusion of map over zipWith
-- Law: map f (zipWith g xs ys) = zipWith (\x y -> f (g x y)) xs ys
forwardLayer :: (Float -> Float) -> Vector Float -> Layer -> Vector Float
forwardLayer activationFunc inputs (!weights, !biases) =
    V.zipWith (\ws b -> activationFunc (dotProd inputs ws + b)) weights biases


-- Forward pass for all layers
forwardAll :: (Float -> Float) -> [Layer] -> Vector Float -> Vector Float
forwardAll activationFunc layers inputs =
    foldl' (forwardLayer activationFunc) inputs layers


-- Sigmoid activation function
sigmoid :: Float -> Float
sigmoid x = 1 / (1 + exp (-x))


writeFlatLayers :: FilePath -> FlatLayers -> IO ()
writeFlatLayers path flatLayers = do
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path (encode flatLayers)
    -- putStrLn $ "FlatLayers written to " ++ path


readFlatLayers :: FilePath -> IO FlatLayers
readFlatLayers path = do
    contents <- BS.readFile path
    case decode contents of
        Left err -> error $ "Failed to decode FlatLayers: " ++ err
        Right flatLayers -> do
            putStrLn $ "FlatLayers read from " ++ path
            return flatLayers

-- Redefine uniformListR from random v1.3.0
-- which I can't use because of QuickCheck dependency :(
uniformListR :: (Random a) => Int -> (a, a) -> StdGen -> ([a], StdGen)
uniformListR n range gen = go n range gen []
    where
        go 0 _ g acc = (reverse acc, g)
        go count r g acc =
            let (val, nextGen) = randomR r g
            in  go (count - 1) r nextGen (val : acc)


vecOfVecsToListOfLists :: Vector (Vector a) -> [[a]]
vecOfVecsToListOfLists vecOfVecs = vecOfVecs & V.toList & map V.toList


listOfListsToVecOfVecs :: [[a]] -> Vector (Vector a)
listOfListsToVecOfVecs listOfLists = listOfLists & map V.fromList & V.fromList


flattenOneLayer :: Layer -> ([Float], (Int, Int))
flattenOneLayer (weights, biases) =
    let (!weights', !biases') = (vecOfVecsToListOfLists weights, V.toList biases)
    in  (concat weights' ++ biases', (length weights', length (head weights')))


-- Flatten all layers into a single list of Floats plus shape info.
flattenLayers :: [Layer] -> FlatLayers
flattenLayers layers =
    let (!flatWeightsAndBiases, !shapes) = layers & map flattenOneLayer & unzip
    in  (concat flatWeightsAndBiases, shapes)


splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = case xs of
    [] -> []
    _ -> take n xs : splitEvery n (drop n xs)


unflattenLayersToLists :: FlatLayers -> [([[Float]], [Float])]
unflattenLayersToLists (flatWeightsAndBiases, shapes) =
    case (flatWeightsAndBiases, shapes) of
        ([], _) -> []
        (_, []) -> []
        (xs, (m, n) : rest) ->
            let (!wVals, !r1) = splitAt (m * n) xs
                (!bVals, !r2) = splitAt m r1
                wMatrix = splitEvery n wVals
                layer = (wMatrix, bVals)
            in  layer : unflattenLayersToLists (r2, rest)


unflattenLayers :: FlatLayers -> [Layer]
unflattenLayers flatLayers =
    let layers = unflattenLayersToLists flatLayers
    in  layers & map (\(w, b) -> (listOfListsToVecOfVecs w, V.fromList b))


pairs :: [a] -> [(a, a)]
pairs xs = zip xs (drop 1 xs)


initFlatLayers :: [Int] -> Float -> StdGen -> (FlatLayers, StdGen)
initFlatLayers layerSizes range gen =
    let shapes = layerSizes & pairs & map swap
        totalLength = sum (map (\(m, n) -> m * n + m) shapes)
        (!weightsAndBiases, !gen') = uniformListR totalLength (-range, range) gen
    in  ((weightsAndBiases, shapes), gen')


-- Genetic Algorithm --
-- TODO This should be a separate module

-- Crossover two flat layers with a probability of 50% that a gene will come from either parent.
crossover :: FlatLayers -> FlatLayers -> StdGen -> (FlatLayers, StdGen)
crossover (parent1, shapes1) (parent2, shapes2) gen =
    let (!probs, !gen') = uniformListR (length parent1) (0, 1) gen :: ([Float], StdGen)
        child = zipWith3 (\p x y -> if p < 0.5 then x else y) probs parent1 parent2
    in  ((child, shapes1), gen')


mutate :: Float -> Float -> (FlatLayers, StdGen) -> (FlatLayers, StdGen)
mutate mutationRate range ((flatLayers, shapes), gen) =
    let (!probs, !gen') = uniformListR (length flatLayers) (0, 1) gen :: ([Float], StdGen)
        -- TODO Use a normal distribution here instead of uniform
        (!offsets, !gen'') = uniformListR (length flatLayers) (-range, range) gen'
        mutated = zipWith3 (\p x y -> if p < mutationRate then y + x else y) probs offsets flatLayers
    in  ((mutated, shapes), gen'')


invert :: Float -> (FlatLayers, StdGen) -> (FlatLayers, StdGen)
invert mutationRate ((flatLayers, shapes), gen) =
    let (!probs, !gen') = uniformListR (length flatLayers) (0, 1) gen :: ([Float], StdGen)
        inverted = zipWith (\p x -> if p < mutationRate then let val = x * (-1) in val else x) probs flatLayers
    in  inverted `seq` ((inverted, shapes), gen')


clamp :: Float -> Float -> Float -> Float
clamp minVal maxVal x = max minVal (min maxVal x)


mutate' :: Float -> Float -> Float -> FlatLayers -> StdGen -> (FlatLayers, StdGen)
mutate' mutationRate range weight (flatLayers, shapes) gen =
    let (!probs, !gen') = uniformListR (length flatLayers) (0, 1) gen :: ([Float], StdGen)
        (!randVals, !gen'') = uniformListR (length flatLayers) (-range * weight, range * weight) gen'
        mutated = zipWith3 (\p x y -> if p < mutationRate then clamp (-range) range (x + y) else y) probs randVals flatLayers
    in  ((mutated, shapes), gen'')
