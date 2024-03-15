{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}


module Ant where

import           Data.Fixed    (mod')
import           System.Random (StdGen, mkStdGen, newStdGen, randomR, randoms)

data Ant = Ant { antX     :: Float,
                 antY     :: Float,
                 antTheta :: Float, -- in radians
                 antMode  :: Mode,
                 antRng   :: StdGen }
                 deriving (Eq, Show)

data Mode = SeekFood | SeekNest deriving (Eq, Show)

type RngSeed = Int

mkAnt :: Float -> Float -> RngSeed -> Ant
mkAnt x y seed =
    let (theta, rng) = randomR (0, 2 * pi) (mkStdGen seed)
    in Ant x y theta SeekFood rng

mkAnts :: Float -> Float -> [RngSeed] -> [Ant]
mkAnts x y seeds = map (mkAnt x y) seeds

printAnts :: [Ant] -> IO ()
printAnts = putStrLn . unlines . map show

step :: Float -> Ant -> Ant
step stepSize ant =
    let theta = antTheta ant
        x' = antX ant + stepSize * cos theta
        y' = antY ant + stepSize * sin theta
    in ant { antX = x', antY = y' }

-- Rotate the ant by the given angle in radians wrapping around if needed
rotate :: Float -> Ant -> Ant
rotate angle ant = ant { antTheta = (antTheta ant + angle) `mod'` (2 * pi) }



-- Reflect the ant theta about the normal vector
reflect :: Float -> Float -> Ant -> Ant
reflect nx ny ant =
    let mag = sqrt (nx^2 + ny^2)
        (nx', ny') = (nx / mag, ny / mag)
        theta = antTheta ant
        (dx, dy) = (cos theta, sin theta)
        dot = dx * nx' + dy * ny'
        (rx, ry) = (dx - 2 * dot * nx', dy - 2 * dot * ny')
    in ant { antTheta = atan2 ry rx `mod'` (2 * pi) }


turnAround :: Ant -> Ant
turnAround ant = rotate pi ant


spawnAnt :: Float -> Float -> RngSeed -> [Ant] -> [Ant]
spawnAnt x y seed ants = mkAnt x y seed : ants


-- TODO Consider leaving the squished ants in a dead state.
-- Maybe other ants can freak out whenever they encounter a dead ant.
squishAnts :: Float -> Float -> [Ant] -> [Ant]
squishAnts x y ants = filter (not . \a -> antX a == x && antY a == y) ants



-- TODO Move nest




-- ======== TESTS =========

testMkAnts :: IO ()
testMkAnts = do
    gen <- newStdGen
    let seeds = take 10 $ randoms gen :: [Int]
    printAnts $ mkAnts 0 0 seeds
