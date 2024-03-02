{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}



module Model where

import           Convolve      (convolve2DSeparable)
import           Data.List     (foldl')
import qualified Data.Matrix   as M
import           System.Random (StdGen, randomR)

data Ant = Ant { antId            :: Int,
                 antX             :: Int,
                 antY             :: Int,
                 antDir           :: Direction,
                 antMode          :: Mode,
                 antFoodPheremone :: FoodPheremone,
                 antNestPheremone :: NestPheremone}
                 deriving (Eq, Show)

data Mode = SeekFood | SeekNest deriving (Eq, Show)

data Direction
    = North
    | Northeast
    | East
    | Southeast
    | South
    | Southwest
    | West
    | Northwest
    deriving (Eq, Enum, Bounded, Show)


type FoodPheremone = Float
type NestPheremone = Float
type FoodUnits = Float

data Patch
    = Border
    | Wall
    | Nest
    | Food FoodUnits
    | Ground FoodPheremone NestPheremone
    deriving (Eq, Show)

type PatchWithPos = (Patch, (Int, Int))

showPatch :: Patch -> Char
showPatch p = case p of
    Border     -> 'B'
    Wall       -> 'W'
    Nest       -> 'N'
    Food _     -> 'F'
    Ground _ _ -> '.'


type Grid = M.Matrix Patch
type PatchWithPosGrid = M.Matrix PatchWithPos
type Neighborhood = M.Matrix Patch
type AntLook = (Patch, Patch, Patch)
type State = (Grid, [Ant], StdGen)

mkGrid :: Int -> Int -> Grid
mkGrid w h = M.matrix h w $ const $ Ground 0 0


showGrid :: Grid -> String
showGrid = unlines . M.toLists . fmap showPatch


getPatch ::Int -> Int -> Grid -> Patch
getPatch x y = M.getElem y x


setPatch :: Patch -> (Int, Int) -> Grid -> Grid
setPatch p (x, y) = M.setElem p (y, x)

setPatchState :: Patch -> (Int, Int) -> State -> State
setPatchState p (x, y) (g, ants, gen) = (setPatch p (x, y) g, ants, gen)

drawPatch :: Int -> Int -> Patch -> Grid -> Grid
drawPatch x y p g = case getPatch x y g of
    Border -> g
    Nest   -> g
    _      -> setPatch p (x, y) g


dryGrid :: Grid -> Grid
dryGrid = fmap dryPatch
    where
        dryPatch :: Patch -> Patch
        dryPatch p = case p of
            Ground f n -> Ground (max 0 (f-1)) (max 0 (n-1))
            Food 0     -> Ground 0 0
            _          -> p

getPatchWithPosGrid :: Grid -> PatchWithPosGrid
getPatchWithPosGrid g = M.matrix (M.nrows g) (M.ncols g) $ \(y, x) -> (getPatch x y g, (x, y))



diffusePatch :: PatchWithPos -> Patch
diffusePatch = undefined


diffuseGrid :: Grid -> Grid
diffuseGrid = fmap diffusePatch . getPatchWithPosGrid


-- diffuseGrid :: Grid -> Grid
-- diffuseGrid g = M.matrix (M.nrows g) (M.ncols g) $ \(y, x) ->
--     let p = getPatch x y g
--     in case p of
--         Ground f n -> let (f', n') = diffusePheremones (x, y) g
--                       in Ground f' n'
--         _          -> p
--     where
--         diffusePheremones :: (Int, Int) -> Grid -> (FoodPheremone, NestPheremone)
--         diffusePheremones (x, y) g =
--             let n = getNeighborhood x y g
--                 centerWeight = 100 -- Adjust this value to control the diffusion rate
--                 totalWeight = centerWeight + 8 -- Center weight + 8 neighbors
--                 (f, n') = foldl' (countPheremones (x, y) g) (0, 0) n
--                 -- Add the center patch's pheromones, weighted more heavily
--                 (cf, cn) = getCenterPheremones x y g
--             in ((f + cf * centerWeight) / totalWeight, (n' + cn * centerWeight) / totalWeight)

--         countPheremones :: (Int, Int) -> Grid -> (FoodPheremone, NestPheremone) -> Patch -> (FoodPheremone, NestPheremone)
--         countPheremones (cx, cy) g (f, n) p = case p of
--             Ground f' n' -> (f + f', n + n')
--             _            -> (f, n)

--         getCenterPheremones :: Int -> Int -> Grid -> (FoodPheremone, NestPheremone)
--         getCenterPheremones x y g = case getPatch x y g of
--             Ground f n -> (f, n)
--             _          -> (0, 0)


setBorder :: Grid -> Grid
setBorder g =
    let (w, h) = (M.ncols g, M.nrows g)
        top = foldl' (\g' x -> drawPatch x 1 Border g') g [1..w]
        bottom = foldl' (\g' x -> drawPatch x h Border g') top [1..w]
        left = foldl' (\g' y -> drawPatch 1 y Border g') bottom [1..h]
        right = foldl' (\g' y -> drawPatch w y Border g') left [1..h]
    in right


setNest :: Grid -> Grid
setNest g =
    let (w, h) = (M.ncols g, M.nrows g)
        (cx, cy) = (w `div` 2, h `div` 2)
    in setPatch Nest (cx+1, cy+1) g


initGrid :: Int -> Int -> Grid
initGrid w h = setNest $ setBorder $ mkGrid w h
-- putStrLn $ showGrid $ initGrid 20 10


initState :: Int -> Int -> Int -> StdGen -> State
initState w h numAnts gen =
    let grid = initGrid w h
        (ants, gen') = mkAnts (w `div` 2) (h `div` 2) gen numAnts
    in (grid, ants, gen')

showAnts :: [Ant] -> String
showAnts = unlines . map show

showState :: State -> String
showState (g, ants, gen) = show g ++ "\n" ++ showAnts ants ++ "\n" ++ show gen
-- putStrLn $ showState $ initState 5 7 1 (mkStdGen 0)

randomDir :: StdGen -> (Direction, StdGen)
randomDir gen =
    let minDir = fromEnum (minBound :: Direction)
        maxDir = fromEnum (maxBound :: Direction)
        (randIndex, nextGen) = randomR (minDir, maxDir) gen
    in (toEnum randIndex, nextGen)


pheromoneAmount :: Float
pheromoneAmount = 300

mkAnt :: Int -> Int -> Int -> StdGen -> (Ant, StdGen)
mkAnt id' x y gen =
    let (dir, gen') = randomDir gen
    in (Ant id' (x+1) (y+1) dir SeekFood 0 pheromoneAmount, gen')


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

turnAround :: Direction -> Direction
turnAround d' = toEnum $ (fromEnum d' + 4) `mod` 8

goStraight :: Direction -> Direction
goStraight = id


randomNextDir :: Direction -> StdGen -> (Direction, StdGen)
randomNextDir d g =
    let (rand :: Int, g') = randomR (1, 12) g
    in if | rand == 1               -> (turnLeft d, g')
          | rand >= 2 && rand <= 11 -> (goStraight d, g')
          | rand == 12              -> (turnRight d, g')
          | otherwise               -> error "Impossible"


patchFood :: Patch -> FoodPheremone
patchFood p = case p of
    Ground f _ -> f
    Food _     -> 1_000_000_000
    _          -> 0

patchNest :: Patch -> NestPheremone
patchNest p = case p of
    Ground _ n -> n
    Nest       -> 1_000_000_000
    _          -> 0

maxFoodDirection :: AntLook -> Direction -> Maybe Direction
maxFoodDirection (l, c, r) dir =
    let (lf, cf, rf) = (patchFood l, patchFood c, patchFood r)
        maxFood = maximum [lf, cf, rf]
    in if maxFood == 0 then Nothing
        else Just $ if | maxFood == lf -> turnLeft   dir
                       | maxFood == cf -> goStraight dir
                       | maxFood == rf -> turnRight  dir


maxNestDirection :: AntLook -> Direction -> Maybe Direction
maxNestDirection (l, c, r) dir =
    let (ln, cn, rn) = (patchNest l, patchNest c, patchNest r)
        maxNest = maximum [ln, cn, rn]
    in if maxNest == 0 then Nothing
        else Just $ if | maxNest == ln -> turnLeft   dir
                       | maxNest == cn -> goStraight dir
                       | maxNest == rn -> turnRight  dir



dropPheremone :: Patch -> Ant -> (Ant, Patch)
dropPheremone p a =
    let (antFood, antNest) = (antFoodPheremone a, antNestPheremone a)
    in case p of
        Ground f n -> case antMode a of
            SeekNest -> (a {antFoodPheremone = max 0 (antFood*0.99)}, Ground (f + antFood) n)
            SeekFood -> (a {antNestPheremone = max 0 (antNest*0.99)}, Ground f (n + antNest))
        _          -> (a,p)



dropPheremones :: Grid -> [Ant] -> ([Ant], Grid)
dropPheremones g ants = foldl' dropPheremone' ([], g) ants
    where
        dropPheremone' :: ([Ant], Grid) -> Ant -> ([Ant], Grid)
        dropPheremone' (ants', g') a =
            let (x, y) = (antX a, antY a)
                p = getPatch x y g'
                (a', p') = dropPheremone p a
            in (a':ants', setPatch p' (x, y) g')

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

lookAtPatch :: Int -> Int -> Direction -> Grid -> Patch
lookAtPatch x y dir g = let (x', y') = step x y dir in getPatch x' y' g

antLook :: Ant -> Grid -> AntLook
antLook a g =
    let (x, y, dir) = (antX a, antY a, antDir a)
    in (lookAtPatch x y (turnLeft   dir) g,
        lookAtPatch x y (goStraight dir) g,
        lookAtPatch x y (turnRight  dir) g)

getNeighborhood :: Int -> Int -> Grid -> Neighborhood
getNeighborhood x y g = M.submatrix (y - 1) (y + 1) (x - 1) (x + 1) g


stepAnt :: Grid -> Ant -> StdGen -> (Grid, Ant, StdGen)
stepAnt g a gen =
    let (x, y, direction) = (antX a, antY a, antDir a)
        look = antLook a g
        (dir, gen') = case antMode a of
            SeekFood -> case maxFoodDirection look direction of
                Just d  -> (d, gen)
                Nothing -> randomNextDir (antDir a) gen
            SeekNest -> case maxNestDirection look direction of
                Just d  -> (d, gen)
                Nothing -> randomNextDir (antDir a) gen
        (dir', gen''') =
            let (r, gen'') = randomR (0.0, 1.0 :: Double) gen'
            in if r < 0.01 then randomNextDir dir gen'' else (dir, gen'')
        (x', y') = step x y dir'
        p = getPatch x' y' g
        a' = case p of
            Food _      -> a {antX = x', antY = y', antDir = turnAround dir', antMode = SeekNest, antFoodPheremone = pheromoneAmount, antNestPheremone = 0}
            Nest        -> a {antX = x', antY = y', antDir = turnAround dir', antMode = SeekFood, antFoodPheremone = 0, antNestPheremone = pheromoneAmount}
            Border      -> a {antDir = turnAround dir'}
            Wall        -> a {antDir = turnAround dir'}
            Ground _ _  -> a {antX = x', antY = y', antDir = dir'}
    in (g, a', gen''')

stepAnts :: State -> State
stepAnts (g, ants, gen) = foldl' stepAnt' (g, [], gen) ants
    where
        stepAnt' :: (Grid, [Ant], StdGen) -> Ant -> (Grid, [Ant], StdGen)
        stepAnt' (g', ants', gen') a =
            let (g'', a', gen'') = stepAnt g' a gen'
            in (g'', a':ants', gen'')

updateState :: State -> State
updateState (g, ants, gen) =
    let g' = dryGrid g -- $ diffuseGrid g
        (g'', ants', gen') = stepAnts (g', ants, gen)
        (ants'', g''') = dropPheremones g'' ants'
    in (g''', ants'', gen')


-- import System.Random
-- import GHC.Utils.Misc
-- putStrLn $ showState $ nTimes 1 updateState $ setPatchState (Food 100) (2,2) $ initState 5 7 1 (mkStdGen 0)
