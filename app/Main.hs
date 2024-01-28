{-# LANGUAGE Strict #-}

module Main where

import qualified Data.Vector as V

data Ant = Ant { antX    :: Int,
                 antY    :: Int,
                 antDir  :: Direction,
                 antMode :: Mode}
                 deriving Show

data Mode = SeekFood | SeekNest deriving Show

data Direction
    = North
    | Northeast
    | East
    | Southeast
    | South
    | Southwest
    | West
    | Northwest
    deriving Show

data Patch
    = Border
    | Wall
    | Food
    | Nest
    | Ground{ foodPheremone :: Int,
              nestPheremone :: Int }
    deriving Show

type WidthHeight = (Int, Int)

type Grid = (V.Vector Patch, WidthHeight)

mkGrid :: Int -> Int -> Grid
mkGrid w h = (
    V.replicate (w * h) Ground {foodPheremone=0, nestPheremone=0},
    (w, h))



showPatch :: Patch -> String
showPatch Border       = "B"
showPatch Wall         = "W"
showPatch Food         = "F"
showPatch Nest         = "N"
showPatch (Ground _ _) = "·"


showRow :: Grid -> Int -> String
showRow grid@(_, (w, _)) y =
    concatMap (\x -> showPatch $ getPatch grid x y) [0..w-1]

showGrid :: Grid -> String
showGrid grid@(_, (_, h)) =
    concatMap (\x -> showRow grid x ++ "\n") [0..h-1]


getPatch :: Grid -> Int -> Int -> Patch
getPatch (data_, (w, _)) x y = let i = y * w + x in data_ V.! i

setPatch :: Grid -> Int -> Int -> Patch -> Grid
setPatch (data_, (w, h)) x y patch =
    let i = y * w + x in (data_ V.// [(i, patch)], (w, h))

setBorder :: Grid -> Grid
setBorder grid@(_, (w, h)) =
    let grid' = foldl (\g x -> setPatch g x 0 Border) grid [0..w-1]
        grid'' = foldl (\g x -> setPatch g x (h-1) Border) grid' [0..w-1]
        grid''' = foldl (\g y -> setPatch g 0 y Border) grid'' [0..h-1]
        grid'''' = foldl (\g y -> setPatch g (w-1) y Border) grid''' [0..h-1]
    in grid''''


main :: IO ()
main = do
    putStrLn "Hello, Ant World!"
