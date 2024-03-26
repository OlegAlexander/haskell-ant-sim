{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}


module Ant where

import           Data.Fixed    (div', mod')
import           Data.Function ((&))
import           GHC.Conc      (ThreadStatus)
import           System.Random (StdGen, mkStdGen, newStdGen, randomR, randoms)

-- -------------------------------------------------------------------------- --

data Ant = Ant { antX     :: Float,
                 antY     :: Float,
                 antTheta :: Float, -- in radians
                 antMode  :: Mode,
                 antRng   :: StdGen }
                 deriving (Eq, Show)

data Mode = SeekFood | SeekNest deriving (Eq, Show)

type RngSeed = Int

-- -------------------------------------------------------------------------- --

mkAnt :: Float -> Float -> RngSeed -> Ant
mkAnt x y seed =
    let (theta, rng) = randomR (0, 2 * pi) (mkStdGen seed)
    in Ant x y theta SeekFood rng

mkAnts :: Float -> Float -> [RngSeed] -> [Ant]
mkAnts x y seeds = map (mkAnt x y) seeds

printAnts :: [Ant] -> IO ()
printAnts = putStrLn . unlines . map show

stepAnt :: Float -> Ant -> Ant
stepAnt stepSize ant =
    let theta = antTheta ant
        x' = antX ant + stepSize * cos theta
        y' = antY ant + stepSize * sin theta
    in ant { antX = x', antY = y' }

-- Rotate the ant by the given angle in radians wrapping around if needed
rotateAnt :: Float -> Ant -> Ant
rotateAnt angle ant = ant { antTheta = (antTheta ant + angle) `mod'` (2 * pi) }


-- Reflect the ant theta about the normal vector
reflectAnt :: Float -> Float -> Ant -> Ant
reflectAnt nx ny ant =
    let mag = sqrt (nx^2 + ny^2)
        (nx', ny') = (nx / mag, ny / mag)
        theta = antTheta ant
        (dx, dy) = (cos theta, sin theta)
        dot = dx * nx' + dy * ny'
        (rx, ry) = (dx - 2 * dot * nx', dy - 2 * dot * ny')
    in ant { antTheta = atan2 ry rx `mod'` (2 * pi) }


turnAroundAnt :: Ant -> Ant
turnAroundAnt ant = rotateAnt pi ant


spawnAnt :: Float -> Float -> RngSeed -> [Ant] -> [Ant]
spawnAnt x y seed ants = mkAnt x y seed : ants


-- TODO Consider leaving the squished ants in a dead state.
-- Maybe other ants can freak out whenever they encounter a dead ant.
squishAnts :: Float -> Float -> Float -> [Ant] -> [Ant]
squishAnts x y width ants = filter (not . isSquished) ants
    where
        isSquished ant =
            let x' = antX ant
                y' = antY ant
            in x' > x - width / 2 && x' < x + width / 2
            && y' > y - width / 2 && y' < y + width / 2




-- TODO Move nest

-- TODO Re spatial partitioning. Use a Matrix/Grid to store the pheromone drops
-- but not the ants. The ants will be stored in a list. You can get which cell
-- an ant is in by doing a floor division of the ant's x and y by the cell width
-- and height. Then you only need to see which drops are within the ant's fov in
-- that cell. You can do the same with the nest, food, and walls.



-- ---------------------------------- Tests --------------------------------- --

testMkAnts :: IO ()
testMkAnts = do
    gen <- newStdGen
    let seeds = (randoms gen :: [Int]) & take 10
    mkAnts 0 0 seeds & printAnts
