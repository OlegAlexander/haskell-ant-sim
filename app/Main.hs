module Main where

import qualified Data.Matrix as M

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

showPatch :: Patch -> Char
showPatch p = case p of
    Border     -> 'B'
    Wall       -> 'W'
    Food       -> 'F'
    Nest       -> 'N'
    Ground _ _ -> '.'


type Grid = M.Matrix Patch


mkGrid :: Int -> Int -> Grid
mkGrid w h = M.matrix h w $ const $ Ground 0 0


showGrid :: Grid -> String
showGrid g = unlines $ M.toLists $ fmap showPatch g


getPatch ::Int -> Int -> Grid -> Patch
getPatch x y = M.getElem (y+1) (x+1)


setPatch :: Int -> Int -> Patch -> Grid -> Grid
setPatch x y p = M.setElem p (y+1, x+1)


setBorder :: Grid -> Grid
setBorder g =
    let (w, h) = (M.ncols g, M.nrows g)
        top = foldl (\g' x -> setPatch x 0 Border g') g [0..w-1]
        bottom = foldl (\g' x -> setPatch x (h-1) Border g') top [0..w-1]
        left = foldl (\g' y -> setPatch 0 y Border g') bottom [0..h-1]
        right = foldl (\g' y -> setPatch (w-1) y Border g') left [0..h-1]
    in right

-- putStrLn $ showGrid $ setBorder $ mkGrid 20 10

main :: IO ()
main = do
    putStrLn "Hello, Ant World!"
