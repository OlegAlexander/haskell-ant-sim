{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}


module Ant where

import           Data.Fixed    (div', mod')
import           Data.Function ((&))
import           System.Random (StdGen, mkStdGen, newStdGen, randomR, randoms)

-- -------------------------------------------------------------------------- --

data Ant = Ant { antX        :: Float,
                 antY        :: Float,
                 antTheta    :: Float, -- in radians
                 antSpeed    :: Float,
                 antMode     :: Mode,
                 antRng      :: StdGen,
                 antStopGo   :: StopGo,
                 antWheelPos :: WheelPos,
                 antSprite   :: Sprite }
                 deriving (Eq, Show)

data Mode = SeekFood | SeekNest deriving (Eq, Show)

data StopGo = Stop | Neutral | Go deriving (Eq, Show)

data WheelPos = Left | Center | Right deriving (Eq, Show)

data Sprite = LeftSprite | RightSprite deriving (Eq, Show)

type RngSeed = Int

-- -------------------------------------------------------------------------- --

mkAnt :: Float -> Float -> RngSeed -> Ant
mkAnt x y seed =
    let (theta, rng) = randomR (0, 2 * pi) (mkStdGen seed)
    in Ant x y theta 0 SeekFood rng Neutral Center LeftSprite

mkAnts :: Float -> Float -> [RngSeed] -> [Ant]
mkAnts x y seeds = map (mkAnt x y) seeds

printAnts :: [Ant] -> IO ()
printAnts = putStrLn . unlines . map show

stepAnt :: Float -> Ant -> Ant
stepAnt stepSize ant =
    let theta = antTheta ant
        speed = antSpeed ant
        x' = antX ant + stepSize * speed * cos theta
        y' = antY ant + stepSize * speed * sin theta
    in ant { antX = x', antY = y' }

-- TODO I don't like having the sprite logic in this module. Look into ECS.
-- TODO I don't like the random movement, but it's ok for now.
cycleAntSprite :: Float -> Ant -> Ant
cycleAntSprite maxSpeed ant =
    let speed = antSpeed ant
        (chance, rng') = randomR (0, 1) (antRng ant)
        sprite' = if chance < sqrt(speed / maxSpeed) then
            case antSprite ant of
                LeftSprite  -> RightSprite
                RightSprite -> LeftSprite
            else
                antSprite ant
    in ant { antSprite = sprite', antRng = rng' }



driveAnt :: Float -> Float -> Float -> Float -> Float -> Float -> Ant -> Ant
driveAnt stepSize acceleration deceleration maxSpeed angle jitter ant =
    let rotatedAnt = case antWheelPos ant of
            Ant.Left  -> leftAnt angle ant
            Ant.Right -> rightAnt angle ant
            Center    -> ant
        translatedAnt = case antStopGo rotatedAnt of
            Stop    -> stopAnt deceleration rotatedAnt
            Neutral -> rotatedAnt
            Go      -> goAnt acceleration maxSpeed rotatedAnt
    in translatedAnt & jitterRotation jitter & stepAnt stepSize


-- -------------------------------- Controls -------------------------------- --

goAnt :: Float -> Float -> Ant -> Ant
goAnt acceleration maxSpeed ant =
    let speed' = min maxSpeed (antSpeed ant + acceleration)
    in ant { antSpeed = speed' }

stopAnt :: Float -> Ant -> Ant
stopAnt deceleration ant =
    let speed' = max 0 (antSpeed ant - deceleration)
    in ant { antSpeed = speed' }

leftAnt :: Float -> Ant -> Ant
leftAnt angle ant = rotateAnt (-angle) ant

rightAnt :: Float -> Ant -> Ant
rightAnt angle ant = rotateAnt angle ant

-- -------------------------------------------------------------------------- --

-- Rotate the ant by the given angle in radians wrapping around if needed
rotateAnt :: Float -> Ant -> Ant
rotateAnt angle ant =
    -- if antSpeed ant == 0 then -- Don't rotate in place
    --     ant
    -- else
        let theta' = (antTheta ant + angle) `mod'` (2 * pi)
        in ant { antTheta = theta' }

jitterRotation :: Float -> Ant -> Ant
jitterRotation angleRange ant =
    let (angle, rng') = randomR (-angleRange, angleRange) (antRng ant)
    in rotateAnt (angle * antSpeed ant) ant { antRng = rng' }

-- TODO Real ants stop a lot when exploring.
moveAntRandomly :: Float -> Float -> Float -> Ant -> Ant
moveAntRandomly stepSize angleRange accelerationRange ant =
    let (angle, rng') = randomR (-angleRange, angleRange) (antRng ant)
        (acceleration, rng'') = randomR (-accelerationRange, accelerationRange) rng'
        newSpeed = max 1 (min 2 (antSpeed ant + acceleration)) -- TODO Magic numbers
        movedAnt = rotateAnt angle ant & stepAnt stepSize
    in movedAnt { antRng = rng'', antSpeed = newSpeed }

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

-- TODO Consider having the ant go into a rotating state instead of rotating instantly
turnAroundAnt :: Ant -> Ant
turnAroundAnt ant = rotateAnt pi ant



-- TODO Consider having this be an option. The other option is to reflect of the border.
wrapAroundAnt :: Float -> Float -> Ant -> Ant
wrapAroundAnt w h ant =
    let x = antX ant
        y = antY ant
        x' = if x > right then (x - w) else if x < left then (x + w) else x
        y' = if y > top then (y - h) else if y < bottom then (y + h) else y
    in ant { antX = x', antY = y' }
    where
        top = h/2
        bottom = -(h/2)
        right = w/2
        left = -(w/2)


wrapAroundAntRaylib :: Float -> Float -> Ant -> Ant
wrapAroundAntRaylib w h ant =
    let x = antX ant
        y = antY ant
        x' = if x > w then 0 else if x < 0 then w else x
        y' = if y > h then 0 else if y < 0 then h else y
    in ant { antX = x', antY = y' }



-- TODO Control an ant (different color)
-- TODO Consider having a local mode where the ant stays still and the world moves around it.
-- Alternatively, the ant can move around the world but only its visual field is shown like fog of war.

spawnAnt :: Float -> Float -> RngSeed -> [Ant] -> [Ant]
spawnAnt x y seed ants = mkAnt x y seed : ants


-- TODO Consider leaving the squished ants in a dead state.
-- Maybe other ants can panic whenever they encounter a dead ant.
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


-- TODO Consider making a separate module for the Ant control/states and the AntBrain.
-- Consider using the WorldTurtle module for the ant controls.
-- The AntBrain can even be ML-based and its vision can be FlatWorld-based.



-- ---------------------------------- Tests --------------------------------- --

testMkAnts :: IO ()
testMkAnts = do
    gen <- newStdGen
    let seeds = (randoms gen :: [Int]) & take 10
    mkAnts 0 0 seeds & printAnts
