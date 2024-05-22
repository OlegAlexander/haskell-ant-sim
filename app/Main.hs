{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}

module Main (main) where

-- ------------------------------ PART Imports ------------------------------ --

import Control.Monad (unless, when)
import Data.Fixed (mod')
import Data.Function ((&))
import Data.List (foldl')
import Data.Vector qualified as V
import Debug.Trace (traceShow)
import GHC.Float (int2Float)
import Raylib.Core (
    clearBackground,
    initWindow,
    isKeyDown,
    isKeyPressed,
    setMouseCursor,
    setTargetFPS,
    toggleFullscreen,
    windowShouldClose,
 )
import Raylib.Core.Text (drawFPS)
import Raylib.Core.Textures (drawTexturePro, loadTexture)
import Raylib.Types (
    Color,
    KeyboardKey (KeyDown, KeyF11, KeyLeft, KeyRight, KeyUp),
    MouseCursor (MouseCursorCrosshair),
    Rectangle (Rectangle),
    Texture (texture'height, texture'width),
    Vector2 (Vector2),
 )
import Raylib.Util (drawing)
import Raylib.Util.Colors (lightGray, white)
import Raylib.Util.Math (rad2Deg)
import System.Random (StdGen, mkStdGen, randomIO, randomR)


-- ----------------------------- PART Constants ----------------------------- --

screenWidth :: Int
screenWidth = 1920


screenHeight :: Int
screenHeight = 1080


screenCenterW :: Float
screenCenterW = int2Float screenWidth / 2


screenCenterH :: Float
screenCenterH = int2Float screenHeight / 2


title :: String
title = "Raylib POC"


fps :: Int
fps = 60


antScale :: Float
antScale = 0.3


antMaxSpeed :: Float
antMaxSpeed = 5


antStepSize :: Float
antStepSize = 3


antPng :: String
antPng = "assets/ant.png"


-- -------------------------------- PART Ant -------------------------------- --

data PlayerAnt = PlayerAnt
    { antX :: Float,
      antY :: Float,
      antTheta :: Float, -- in radians
      antSpeed :: Float,
      antMode :: Mode,
      antRng :: StdGen,
      antStopGo :: StopGo,
      antWheelPos :: WheelPos,
      antSprite :: Sprite
    }
    deriving (Eq, Show)


data Mode = SeekFood | SeekNest deriving (Eq, Show)


data StopGo = Stop | Neutral | Go deriving (Eq, Show)


data WheelPos = TurnLeft | Center | TurnRight deriving (Eq, Show)


data Sprite = LeftSprite | RightSprite deriving (Eq, Show)


type RngSeed = Int


-- -------------------------------------------------------------------------- --

mkAnt :: Float -> Float -> RngSeed -> PlayerAnt
mkAnt x y seed =
    let (theta, rng) = randomR (0, 2 * pi) (mkStdGen seed)
    in  PlayerAnt x y theta 0 SeekFood rng Neutral Center LeftSprite


mkAnts :: Float -> Float -> [RngSeed] -> [PlayerAnt]
mkAnts x y seeds = map (mkAnt x y) seeds


printAnts :: [PlayerAnt] -> IO ()
printAnts = putStrLn . unlines . map show


stepAnt :: Float -> PlayerAnt -> PlayerAnt
stepAnt stepSize ant =
    let theta = antTheta ant
        speed = antSpeed ant
        x' = antX ant + stepSize * speed * cos theta
        y' = antY ant + stepSize * speed * sin theta
    in  ant{antX = x', antY = y'}


-- TODO I don't like having the sprite logic in this module. Look into ECS.
-- TODO I don't like the random leg movement, but it's ok for now.
-- TODO The legs should still move when rotating in place.
cycleAntSprite :: Float -> PlayerAnt -> PlayerAnt
cycleAntSprite maxSpeed ant =
    let speed = antSpeed ant
        (chance, rng') = randomR (0, 1) (antRng ant)
        sprite' =
            if chance < sqrt (speed / maxSpeed)
                then case antSprite ant of
                    LeftSprite -> RightSprite
                    RightSprite -> LeftSprite
                else antSprite ant
    in  ant{antSprite = sprite', antRng = rng'}


driveAnt :: Float -> Float -> Float -> Float -> Float -> Float -> PlayerAnt -> PlayerAnt
driveAnt stepSize acceleration deceleration maxSpeed angle jitter ant =
    let rotatedAnt = case antWheelPos ant of
            TurnLeft -> leftAnt angle ant
            TurnRight -> rightAnt angle ant
            Center -> ant
        translatedAnt = case antStopGo rotatedAnt of
            Stop -> stopAnt deceleration rotatedAnt
            Neutral -> rotatedAnt
            Go -> goAnt acceleration maxSpeed rotatedAnt
    in  translatedAnt & jitterRotation jitter & stepAnt stepSize


-- -------------------------------- Controls -------------------------------- --

goAnt :: Float -> Float -> PlayerAnt -> PlayerAnt
goAnt acceleration maxSpeed ant =
    let speed' = min maxSpeed (antSpeed ant + acceleration)
    in  ant{antSpeed = speed'}


stopAnt :: Float -> PlayerAnt -> PlayerAnt
stopAnt deceleration ant =
    let speed' = max 0 (antSpeed ant - deceleration)
    in  ant{antSpeed = speed'}


leftAnt :: Float -> PlayerAnt -> PlayerAnt
leftAnt angle ant = rotateAnt (-angle) ant


rightAnt :: Float -> PlayerAnt -> PlayerAnt
rightAnt angle ant = rotateAnt angle ant


-- -------------------------------------------------------------------------- --

-- Rotate the ant by the given angle in radians wrapping around if needed
rotateAnt :: Float -> PlayerAnt -> PlayerAnt
rotateAnt angle ant =
    -- if antSpeed ant == 0 then -- Don't rotate in place
    --     ant
    -- else
    let theta' = (antTheta ant + angle) `mod'` (2 * pi)
    in  ant{antTheta = theta'}


jitterRotation :: Float -> PlayerAnt -> PlayerAnt
jitterRotation angleRange ant =
    let (angle, rng') = randomR (-angleRange, angleRange) (antRng ant)
    in  rotateAnt (angle * antSpeed ant) ant{antRng = rng'}


-- TODO Real ants stop a lot when exploring.
moveAntRandomly :: Float -> Float -> Float -> PlayerAnt -> PlayerAnt
moveAntRandomly stepSize angleRange accelerationRange ant =
    let (angle, rng') = randomR (-angleRange, angleRange) (antRng ant)
        (acceleration, rng'') = randomR (-accelerationRange, accelerationRange) rng'
        newSpeed = max 1 (min 2 (antSpeed ant + acceleration)) -- TODO Magic numbers
        movedAnt = rotateAnt angle ant & stepAnt stepSize
    in  movedAnt{antRng = rng'', antSpeed = newSpeed}


-- Reflect the ant theta about the normal vector
reflectAnt :: Float -> Float -> PlayerAnt -> PlayerAnt
reflectAnt nx ny ant =
    let mag = sqrt (nx ^ 2 + ny ^ 2)
        (nx', ny') = (nx / mag, ny / mag)
        theta = antTheta ant
        (dx, dy) = (cos theta, sin theta)
        dot = dx * nx' + dy * ny'
        (rx, ry) = (dx - 2 * dot * nx', dy - 2 * dot * ny')
    in  ant{antTheta = atan2 ry rx `mod'` (2 * pi)}


-- TODO Consider having the ant go into a rotating state instead of rotating instantly
turnAroundAnt :: PlayerAnt -> PlayerAnt
turnAroundAnt ant = rotateAnt pi ant


-- TODO Consider having this be an option. The other option is to reflect of the border.
wrapAroundAnt :: Float -> Float -> PlayerAnt -> PlayerAnt
wrapAroundAnt w h ant =
    let x = antX ant
        y = antY ant
        x' = if x > right then (x - w) else if x < left then (x + w) else x
        y' = if y > top then (y - h) else if y < bottom then (y + h) else y
    in  ant{antX = x', antY = y'}
    where
        top = h / 2
        bottom = -(h / 2)
        right = w / 2
        left = -(w / 2)


wrapAroundAntRaylib :: Float -> Float -> PlayerAnt -> PlayerAnt
wrapAroundAntRaylib w h ant =
    let x = antX ant
        y = antY ant
        x' = if x > w then 0 else if x < 0 then w else x
        y' = if y > h then 0 else if y < 0 then h else y
    in  ant{antX = x', antY = y'}


-- TODO Control an ant (different color)
-- TODO Consider having a local mode where the ant stays still and the world moves around it.
-- Alternatively, the ant can move around the world but only its visual field is shown like fog of war.
spawnAnt :: Float -> Float -> RngSeed -> [PlayerAnt] -> [PlayerAnt]
spawnAnt x y seed ants = mkAnt x y seed : ants


-- TODO Consider leaving the squished ants in a dead state.
-- Maybe other ants can panic whenever they encounter a dead ant.
squishAnts :: Float -> Float -> Float -> [PlayerAnt] -> [PlayerAnt]
squishAnts x y width ants = filter (not . isSquished) ants
    where
        isSquished ant =
            let x' = antX ant
                y' = antY ant
            in  x'
                    > x
                        - width
                            / 2
                    && x'
                        < x
                            + width
                                / 2
                    && y'
                        > y
                            - width
                                / 2
                    && y'
                        < y
                            + width
                                / 2


-- TODO Move nest

-- TODO Re spatial partitioning. Use a Matrix/Grid to store the pheromone drops
-- but not the ants. The ants will be stored in a list. You can get which cell
-- an ant is in by doing a floor division of the ant's x and y by the cell width
-- and height. Then you only need to see which drops are within the ant's fov in
-- that cell. You can do the same with the nest, food, and walls.

-- TODO Consider making a separate module for the Ant control/states and the AntBrain.
-- Consider using the WorldTurtle module for the ant controls.
-- The AntBrain can even be ML-based and its vision can be FlatWorld-based.

-- -------------------------------- PART ECS -------------------------------- --

-- ------------------------------- Components ------------------------------- --

-- data EntityType
--     = PlayerAnt
--     | Ant
--     | DeadAnt
--     | Pheromone
--     | Food
--     | Nest
--     | Rock
--     deriving (Eq, Show)

-- data AntMode = SeekFood | SeekNest deriving (Eq, Show)

-- data PedalPos = Decelerate | Neutral | Accelerate deriving (Eq, Show)

-- data WheelPos = TurnLeft | Center | TurnRight deriving (Eq, Show)

-- data AntSprite = LeftSprite | RightSprite deriving (Eq, Show)

-- type RngSeed = Int

-- type Position = Vector2

-- type Angle = Float

-- -- type Speed = Float

-- -- --------------------------------- Entity --------------------------------- --

-- data Entity = Entity
--     { eType :: EntityType,
--       ePos :: Position,
--       eAngle :: Angle,
--       eSpeed :: Speed,
--       eMode :: AntMode,
--       eRng :: StdGen,
--       eStopGo :: PedalPos,
--       eWheel :: WheelPos,
--       eSprite :: AntSprite
--     }
--     deriving (Eq, Show)

-- defaultEntity :: Entity
-- defaultEntity =
--     Entity
--         { eType = Ant,
--           ePos = Vector2 0 0,
--           eAngle = 0,
--           eSpeed = 0,
--           eMode = SeekFood,
--           eRng = undefined,
--           eStopGo = Neutral,
--           eWheel = Center,
--           eSprite = LeftSprite
--         }

-- type World = V.Vector Entity

-- -- --------------------------- Entity Constructors -------------------------- --

-- mkPlayerAnt :: Position -> RngSeed -> Entity
-- mkPlayerAnt pos seed =
--     defaultEntity{eType = PlayerAnt, ePos = pos, eRng = mkStdGen seed}

-- mkWorld :: [RngSeed] -> World
-- mkWorld seeds =
--     let (playerAntSeed, antSeeds) = case seeds of
--             [] -> error "mkWorld: empty seed list"
--             (x : xs) -> (x, xs)
--     in  V.singleton $ mkPlayerAnt (Vector2 0 0) playerAntSeed

-- ----------------------------- Fold World Test ---------------------------- --

data WorldState = WorldState
    { nextEntityId :: Int,
      entities :: [Int]
    }
    deriving (Eq, Show)


newEntity :: WorldState -> () -> WorldState
newEntity (WorldState x es) _ = WorldState (x + 1) (es ++ [x])


mkWorldState :: Int -> WorldState
mkWorldState n = foldl' newEntity (WorldState 1 []) (replicate n ())


-- ------------------------------- PART Utils ------------------------------- --

-- TODO Recommend this function to Raylib author
drawTextureCentered :: Texture -> Rectangle -> Float -> Float -> Vector2 -> Color -> IO ()
drawTextureCentered texture source@(Rectangle _ _ w h) scale angle (Vector2 x y) color = do
    let w' = w * scale
        h' = h * scale
    drawTexturePro
        texture
        source
        (Rectangle x y w' h')
        (Vector2 (w' / 2) (h' / 2)) -- center of the scaled rect
        angle
        color


-- ----------------------------- PART Game Loop ----------------------------- --

data World = World
    { wAntTexture :: Texture,
      wPlayerAnt :: PlayerAnt,
      wShouldExit :: Bool
    }


initWorld :: IO World
initWorld = do
    seed <- randomIO
    let ant = mkAnt screenCenterW screenCenterH seed
    window <- initWindow screenWidth screenHeight title
    setTargetFPS fps
    setMouseCursor MouseCursorCrosshair
    antTexture <- loadTexture antPng window
    return (World antTexture ant False)


handleInput :: World -> IO World
handleInput (World tex ant exit) = do
    go <- isKeyDown KeyUp
    stop <- isKeyDown KeyDown
    left <- isKeyDown KeyLeft
    right <- isKeyDown KeyRight
    let ant' =
            ant
                { antStopGo = if go then Go else if stop then Stop else Neutral,
                  antWheelPos = if left then TurnLeft else if right then TurnRight else Center
                }
    exit' <- windowShouldClose
    return (World tex ant' exit')


updateWorld :: World -> World
updateWorld (World antTexture ant exit) =
    let ant' =
            ant
                & driveAnt antStepSize 0.33 0.33 antMaxSpeed (pi / 15) (pi / 60)
                & cycleAntSprite antMaxSpeed
                & wrapAroundAntRaylib (int2Float screenWidth) (int2Float screenHeight)
    in  World antTexture ant' exit


renderWorld :: World -> IO ()
renderWorld (World antTexture ant exit) = do
    f11Pressed <- isKeyPressed KeyF11
    when f11Pressed toggleFullscreen

    drawing $ do
        let texW = texture'width antTexture
            texH = texture'height antTexture
            sprite = antSprite ant
            spriteRect = case sprite of
                LeftSprite -> Rectangle 0 0 (int2Float texW / 2) (int2Float texH)
                RightSprite -> Rectangle (int2Float texW / 2) 0 (int2Float texW / 2) (int2Float texH)
        clearBackground lightGray
        drawTextureCentered
            antTexture
            spriteRect
            antScale
            (antTheta ant * rad2Deg)
            (Vector2 (antX ant) (antY ant))
            white
        drawFPS 10 10


shouldExit :: World -> Bool
shouldExit w = wShouldExit w


-- A generic game loop!
gameLoop :: (w -> IO w) -> (w -> w) -> (w -> IO ()) -> (w -> Bool) -> w -> IO ()
gameLoop handleInputFunc updateFunc renderFunc shouldExitFunc world = do
    world' <- handleInputFunc world
    unless (shouldExitFunc world') $ do
        let world'' = updateFunc world'
        renderFunc world''
        gameLoop handleInputFunc updateFunc renderFunc shouldExitFunc world''


main :: IO ()
main = initWorld >>= gameLoop handleInput updateWorld renderWorld shouldExit
