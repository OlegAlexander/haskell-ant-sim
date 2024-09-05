{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}

module MainGame where

-- ------------------------------ PART Imports ------------------------------ --

import Control.Monad (when)
import Data.Fixed (mod')
import Data.Function ((&))
import Data.List (foldl')

-- import Debug.Trace (trace, traceShow)

import Constants (
    antAcceleration,
    antJitterAngle,
    antMaxSpeed,
    antPng,
    antScale,
    antTurnAngle,
    borderWallThickness,
    fps,
    screenHeight,
    screenWidth,
    title,
 )
import DrawWalls (drawWallsSys1)
import FlatlandRenderer (flatlandRendererSys)
import GHC.Float (int2Float)
import Raylib.Core (
    clearBackground,
    initWindow,
    isKeyDown,
    isKeyPressed,
    setMouseCursor,
    setTargetFPS,
    setTraceLogLevel,
    toggleFullscreen,
    windowShouldClose,
 )
import Raylib.Core.Text (drawFPS)
import Raylib.Core.Textures (drawTexturePro, loadTexture)
import Raylib.Types (
    Color,
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
    Rectangle (Rectangle),
    Texture (texture'height, texture'width),
    TraceLogLevel (LogWarning),
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (lightGray, white)
import Raylib.Util.Math (deg2Rad, rad2Deg)
import Shared (System (..), gameLoop)
import System.Random (mkStdGen, randomIO, randomR)
import Types (Ant (..), GoDir (..), Mode (SeekFood), Sprite (LeftSprite, RightSprite), WheelPos (Center, TurnLeft, TurnRight), World (..))


-- ----------------------------- PART Constants ----------------------------- --

borderWalls :: [Rectangle]
borderWalls =
    let t = borderWallThickness
        w = int2Float screenWidth
        h = int2Float screenHeight
    in  [ Rectangle (-t) (-t) (w + t * 2) t,
          Rectangle w (-t) t (h + t * 2),
          Rectangle (-t) h (w + t * 2) t,
          Rectangle (-t) (-t) t (h + t * 2)
        ]


-- ----------------------------- PART Player Ant ---------------------------- --

mkAnt :: Float -> Float -> Int -> Ant
mkAnt x y seed =
    let (angle, rng) = randomR (0, 360) (mkStdGen seed)
    in  Ant (Vector2 x y) angle 0 SeekFood rng Stop Center LeftSprite [] 0 0 False


mkAnts :: Float -> Float -> [Int] -> [Ant]
mkAnts x y seeds = map (mkAnt x y) seeds


stepAnt :: Vector2 -> Ant -> Ant
stepAnt nextPos ant = ant{antPos = nextPos}


getNextAntPos :: Ant -> Vector2
getNextAntPos ant =
    let angle = antAngle ant * deg2Rad
        speed = antSpeed ant
        Vector2 x y = antPos ant
        x' = x + speed * cos angle
        y' = y + speed * sin angle
    in  Vector2 x' y'


-- TODO I don't like having the sprite logic in this module. Look into ECS.
-- TODO I don't like the random leg movement, but it's ok for now.
-- TODO The legs should still move when rotating in place.
cycleAntSprite :: Ant -> Ant
cycleAntSprite ant =
    let speed = antSpeed ant
        (chance, rng') = randomR (0, 1) (antRng ant)
        sprite' =
            if chance < sqrt (speed / antMaxSpeed)
                then case antSprite ant of
                    LeftSprite -> RightSprite
                    RightSprite -> LeftSprite
                else antSprite ant
    in  ant{antSprite = sprite', antRng = rng'}


driveAnt :: [Rectangle] -> Ant -> Ant
driveAnt walls ant =
    let rotatedAnt = case antWheelPos ant of
            TurnLeft -> leftAnt ant
            TurnRight -> rightAnt ant
            Center -> ant
        setSpeedAnt =
            case antGoDir rotatedAnt of
                Forward -> goAnt rotatedAnt
                Stop -> stopAnt rotatedAnt
                Backward -> backAnt rotatedAnt
        nextAntPos = getNextAntPos setSpeedAnt
        translatedAnt =
            if checkWallCollision walls nextAntPos
                then setSpeedAnt & jitterRotation & stepAnt nextAntPos
                else setSpeedAnt
    in  translatedAnt


-- -------------------------------- Controls -------------------------------- --

goAnt :: Ant -> Ant
goAnt ant =
    let speed' = min antMaxSpeed (antSpeed ant + antAcceleration)
    in  ant{antSpeed = speed'}


stopAnt :: Ant -> Ant
stopAnt ant =
    let speed' = max 0 (antSpeed ant - antAcceleration)
    in  ant{antSpeed = speed'}


backAnt :: Ant -> Ant
backAnt ant =
    let speed' = max (-antMaxSpeed) (antSpeed ant - antAcceleration)
    in  ant{antSpeed = speed'}


leftAnt :: Ant -> Ant
leftAnt ant = rotateAnt (-antTurnAngle) ant


rightAnt :: Ant -> Ant
rightAnt ant = rotateAnt antTurnAngle ant


-- -------------------------------- Collision ------------------------------- --

canGoThere :: Vector2 -> Rectangle -> Bool
canGoThere (Vector2 x y) (Rectangle rx ry rw rh) =
    x < rx || x > rx + rw || y < ry || y > ry + rh


checkWallCollision :: [Rectangle] -> Vector2 -> Bool
checkWallCollision walls nextPos = all (canGoThere nextPos) walls


-- -------------------------------------------------------------------------- --

-- Rotate the ant by the given angle in degrees wrapping around if needed
rotateAnt :: Float -> Ant -> Ant
rotateAnt angle ant =
    let angle' = (antAngle ant + angle) `mod'` 360
    in  ant{antAngle = angle'}


jitterRotation :: Ant -> Ant
jitterRotation ant =
    let (angle, rng') = randomR (-antJitterAngle, antJitterAngle) (antRng ant)
    in  rotateAnt (angle * antSpeed ant) ant{antRng = rng'}


-- TODO Real ants stop a lot when exploring.
-- moveAntRandomly :: Float -> Float -> Float -> Ant -> Ant
-- moveAntRandomly stepSize angleRange accelerationRange ant =
--     let (angle, rng') = randomR (-angleRange, angleRange) (antRng ant)
--         (acceleration, rng'') = randomR (-accelerationRange, accelerationRange) rng'
--         newSpeed = max 1 (min 2 (antSpeed ant + acceleration)) -- TODO Magic numbers
--         movedAnt = rotateAnt angle ant & stepAnt
--     in  movedAnt{antRng = rng'', antSpeed = newSpeed}

-- Reflect the ant angle about the normal vector
reflectAnt :: Float -> Float -> Ant -> Ant
reflectAnt nx ny ant =
    let mag = sqrt (nx ^ (2 :: Int) + ny ^ (2 :: Int))
        (nx', ny') = (nx / mag, ny / mag)
        angle = antAngle ant * deg2Rad
        (dx, dy) = (cos angle, sin angle)
        dot = dx * nx' + dy * ny'
        (rx, ry) = (dx - 2 * dot * nx', dy - 2 * dot * ny')
    in  ant{antAngle = atan2 ry rx `mod'` (2 * pi) * rad2Deg}


-- TODO Consider having the ant go into a rotating state instead of rotating instantly
turnAroundAnt :: Ant -> Ant
turnAroundAnt ant = rotateAnt 180 ant


-- TODO Control an ant (different color)
-- TODO Consider having a local mode where the ant stays still and the world moves around it.
-- Alternatively, the ant can move around the world but only its visual field is shown like fog of war.
spawnAnt :: Float -> Float -> Int -> [Ant] -> [Ant]
spawnAnt x y seed ants = mkAnt x y seed : ants


-- TODO Consider leaving the squished ants in a dead state.
-- Maybe other ants can panic whenever they encounter a dead ant.
squishAnts :: Float -> Float -> Float -> [Ant] -> [Ant]
squishAnts x y width ants = filter (not . isSquished) ants
    where
        isSquished ant =
            let Vector2 x' y' = antPos ant
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

-- ------------------------------- Components ------------------------------- --

-- data AntMode = SeekFood | SeekNest deriving (Eq, Show)

-- data PedalPos = Decelerate | Neutral | Accelerate deriving (Eq, Show)

-- data WheelPos = TurnLeft | Center | TurnRight deriving (Eq, Show)

-- data AntSprite = LeftSprite | RightSprite deriving (Eq, Show)

-- type RngSeed = Int

-- type Position = Vector2

-- type Angle = Float

-- -- type Speed = Float

-- ---------------------------- PART Constructors --------------------------- --

mkPlayerAnt :: Float -> Float -> Int -> Ant
mkPlayerAnt x y seed =
    let rng = mkStdGen seed
    in  Ant (Vector2 x y) 0 0 SeekFood rng Stop Center LeftSprite [] 0 0 False


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

-- TODO Recommend this function to Raylib author!
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

initWorld :: IO World
initWorld = do
    seed <- randomIO
    let screenCenterW = int2Float screenWidth / 2
        screenCenterH = int2Float screenHeight / 2
        playerAnt = mkPlayerAnt screenCenterW screenCenterH seed
        nestPos = Vector2 screenCenterW screenCenterH
        testWall1 = Rectangle 200 200 500 300
        testWall2 = Rectangle 100 300 1000 50
        testWall3 = Rectangle 500 600 50 50
        walls = [testWall1, testWall2, testWall3] ++ borderWalls
    window <- initWindow screenWidth screenHeight title
    setTargetFPS fps
    setTraceLogLevel LogWarning
    setMouseCursor MouseCursorCrosshair
    antTexture <- loadTexture antPng window
    return $ World window antTexture playerAnt nestPos True True False True walls Nothing [] Nothing


handleWorldInput :: World -> IO World
handleWorldInput w = do
    go <- isKeyDown KeyUp
    left <- isKeyDown KeyLeft
    right <- isKeyDown KeyRight
    let playerAnt' =
            (wPlayerAnt w)
                { antGoDir = Stop,
                  antWheelPos = case (left, right) of
                    (True, False) -> TurnLeft
                    (False, True) -> TurnRight
                    _ -> Center
                }
    return w{wPlayerAnt = playerAnt'}


updateWorld :: World -> World
updateWorld w =
    let walls = wWalls w
        playerAnt' =
            wPlayerAnt w
                & driveAnt walls
                & cycleAntSprite
    in  w{wPlayerAnt = playerAnt'}


renderWorld :: World -> IO ()
renderWorld w = do
    let antTexture = wAntTexture w
        playerAnt = wPlayerAnt w
        texW = texture'width antTexture
        texH = texture'height antTexture
        spriteRect = case antSprite playerAnt of
            LeftSprite -> Rectangle 0 0 (int2Float texW / 2) (int2Float texH)
            RightSprite -> Rectangle (int2Float texW / 2) 0 (int2Float texW / 2) (int2Float texH)
    drawTextureCentered
        antTexture
        spriteRect
        antScale
        (antAngle playerAnt)
        (antPos playerAnt)
        white


mainGameSys :: System World
mainGameSys =
    let allSystems =
            System handleWorldInput updateWorld renderWorld
                <> drawWallsSys1
                <> flatlandRendererSys
    in  allSystems
            { render = \w -> drawing $ do
                f11Pressed <- isKeyPressed KeyF11
                when f11Pressed toggleFullscreen
                clearBackground lightGray
                render allSystems w
                drawFPS 10 10
            }


main :: IO ()
main = initWorld >>= gameLoop mainGameSys windowShouldClose
