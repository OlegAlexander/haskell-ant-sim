{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}



module Main where

import           Data.List     (foldl')
import qualified Data.Matrix   as M
import           System.Random (StdGen, randomR)

data Ant = Ant { antId   :: Int,
                 antX    :: Int,
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
    deriving (Enum, Bounded, Show)


type FoodPheremone = Int
type NestPheremone = Int

data Patch
    = Border
    | Wall
    | Food
    | Nest
    | Ground FoodPheremone NestPheremone
    deriving (Eq, Show)

showPatch :: Patch -> Char
showPatch p = case p of
    Border     -> 'B'
    Wall       -> 'W'
    Food       -> 'F'
    Nest       -> 'N'
    Ground _ _ -> '.'


type Grid = M.Matrix Patch
type Neighborhood = M.Matrix Patch
type State = (Grid, [Ant], StdGen)

mkGrid :: Int -> Int -> Grid
mkGrid w h = M.matrix h w $ const $ Ground 0 0


showGrid :: Grid -> String
showGrid = unlines . M.toLists . fmap showPatch


getPatch ::Int -> Int -> Grid -> Patch
getPatch x y g = M.getElem (y+1) (x+1) g


setPatch :: Int -> Int -> Patch -> Grid -> Grid
setPatch x y p g = M.setElem p (y+1, x+1) g


dryGrid :: Grid -> Grid
dryGrid = fmap dryPatch
    where
        dryPatch :: Patch -> Patch
        dryPatch p = case p of
            Ground f n -> Ground (max 0 (f-1)) (max 0 (n-1))
            _          -> p


setBorder :: Grid -> Grid
setBorder g =
    let (w, h) = (M.ncols g, M.nrows g)
        top = foldl (\g' x -> setPatch x 0 Border g') g [0..w-1]
        bottom = foldl (\g' x -> setPatch x (h-1) Border g') top [0..w-1]
        left = foldl (\g' y -> setPatch 0 y Border g') bottom [0..h-1]
        right = foldl (\g' y -> setPatch (w-1) y Border g') left [0..h-1]
    in right

-- putStrLn $ showGrid $ setBorder $ mkGrid 20 10


randomDir :: StdGen -> (Direction, StdGen)
randomDir gen =
    let minDir = fromEnum (minBound :: Direction)
        maxDir = fromEnum (maxBound :: Direction)
        (randIndex, nextGen) = randomR (minDir, maxDir) gen
    in (toEnum randIndex, nextGen)


mkAnt :: Int -> Int -> Int -> StdGen -> (Ant, StdGen)
mkAnt id' x y gen =
    let (dir, gen') = randomDir gen
    in (Ant id' x y dir SeekFood, gen')


mkAnts :: Int -> Int -> StdGen -> Int -> ([Ant], StdGen)
mkAnts x y gen n = go x y gen n []
    where
        go _ _ g 0 ants = (reverse ants, g)
        go x' y' g n' ants =
            let (ant, g') = mkAnt (n - n' + 1) x' y' g
            in go x' y' g' (n' - 1) (ant:ants)



turnLeft :: Direction -> Direction
turnLeft d' = toEnum $ (fromEnum d' - 1) `mod` 8

turnRight :: Direction -> Direction
turnRight d' = toEnum $ (fromEnum d' + 1) `mod` 8

goStraight :: Direction -> Direction
goStraight = id


randomNextDir :: Direction -> StdGen -> (Direction, StdGen)
randomNextDir d g =
    let (rand :: Int, g') = randomR (0, 4) g
    in if | rand == 0              -> (turnLeft d, g')
          | rand >= 1 && rand <= 3 -> (goStraight d, g')
          | rand == 4              -> (turnRight d, g')
          | otherwise              -> error "Impossible"


dropPheremone :: Patch -> Ant -> Patch
dropPheremone p a = case p of
    Ground f n -> case antMode a of
        SeekFood -> Ground (f + 100) n
        SeekNest -> Ground f (n + 100)
    _          -> p


dropPheremones :: Grid -> [Ant] -> Grid
dropPheremones g ants = foldl' updateGrid g ants
    where
        updateGrid :: Grid -> Ant -> Grid
        updateGrid g' a =
            let (x, y) = (antX a, antY a)
            in setPatch x y (dropPheremone (getPatch x y g') a) g'

step :: Int -> Int -> Direction -> (Int, Int)
step x y dir = case dir of
    North     -> (x, y - 1)
    Northeast -> (x + 1, y - 1)
    East      -> (x + 1, y)
    Southeast -> (x + 1, y + 1)
    South     -> (x, y + 1)
    Southwest -> (x - 1, y + 1)
    West      -> (x - 1, y)
    Northwest -> (x - 1, y - 1)


getNeighborhood :: Int -> Int -> Grid -> Neighborhood
getNeighborhood x y g = M.submatrix (y - 1) (y + 1) (x - 1) (x + 1) g


moveAnt :: Grid -> Ant -> StdGen -> (Ant, StdGen)
moveAnt g a gen =
    let (x, y) = (antX a, antY a)
        (dir, gen') = randomNextDir (antDir a) gen
        (x', y') = step x y dir
        patch = getPatch x' y' g
        mode' = case antMode a of
            SeekFood -> if patch == Nest then SeekNest else SeekFood
            SeekNest -> if patch == Food then SeekFood else SeekNest
    in (Ant (antId a) x' y' dir mode', gen')

main :: IO ()
main = do
    putStrLn "Hello, Ant World!"
