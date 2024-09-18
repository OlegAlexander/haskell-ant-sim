{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use tuple-section" #-}

module AntMovement where

import Constants (antAcceleration, antMaxSpeed, antPng, antTurnAngle, collisionRectSize, fps, screenHeight, screenWidth)
import Control.Monad (forM_, when)
import Data.Fixed (mod')
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShowId)
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
import Raylib.Core.Shapes (
    drawCircleV,
    drawLineEx,
    drawRectangleLinesEx,
    drawRectangleRec,
 )
import Raylib.Core.Text (drawFPS)
import Raylib.Core.Textures (loadTexture)
import Raylib.Types (
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
    Rectangle (Rectangle),
    TraceLogLevel (LogWarning),
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black, blue, brown, green, lightGray, red)
import Shared (System (..), calcCenteredRect, gameLoop, getNextPos)
import System.Random (mkStdGen)
import Types (Ant (..), EntityType (..), Food (..), GoDir (..), Mode (..), Nest (..), Sprite (..), WheelPos (..), World (..))


mkPlayerAnt :: Float -> Float -> Int -> Ant
mkPlayerAnt x y seed =
    let rng = mkStdGen seed
    in  Ant (Vector2 x y) 0 0 SeekFood rng Stop Center LeftSprite [] 0 0 False


canGoThere :: Vector2 -> (Rectangle, EntityType) -> Maybe (Rectangle, EntityType)
canGoThere (Vector2 x y) (rect@(Rectangle rx ry rw rh), entityType) =
    if x < rx || x > rx + rw || y < ry || y > ry + rh
        then Nothing
        else Just (rect, entityType)


checkCollisions :: Vector2 -> [(Rectangle, EntityType)] -> [(Rectangle, EntityType)]
checkCollisions pos rects = mapMaybe (canGoThere pos) rects


getCollisionRects :: World -> [(Rectangle, EntityType)]
getCollisionRects w =
    let walls = map (\wall -> (wall, WallET)) (wWalls w)
        foods = [] -- map (\food -> (foodCollisionRect food, FoodET)) (wFood w)
    in  walls ++ foods


initAMWorld :: IO World
initAMWorld = do
    let screenCenterW = int2Float screenWidth / 2
        screenCenterH = int2Float screenHeight / 2
        testWall1 = Rectangle 200 200 500 300
        testWall2 = Rectangle 100 300 1000 50
        testWall3 = Rectangle 900 500 20 20
        walls = [testWall1, testWall2, testWall3]
    window <- initWindow screenWidth screenHeight "Ant Movement"
    setTargetFPS fps
    setTraceLogLevel LogWarning
    setMouseCursor MouseCursorCrosshair
    antTexture <- loadTexture antPng window
    let rng = mkStdGen 0
        antPos = Vector2 screenCenterW screenCenterH
        nest = Nest antPos 0 (calcCenteredRect antPos collisionRectSize)
        playerAnt = Ant antPos 0 0 SeekFood rng Stop Center LeftSprite [] 0 0 False
    return $ World window antTexture playerAnt nest True True False True walls Nothing [] Nothing


handleAMInput :: World -> IO World
handleAMInput w = do
    up <- isKeyDown KeyUp
    down <- isKeyDown KeyDown
    left <- isKeyDown KeyLeft
    right <- isKeyDown KeyRight
    let playerAnt = wPlayerAnt w
        playerWheelPos =
            antWheelPos playerAnt
                & \_ ->
                    if right then TurnRight else if left then TurnLeft else Center
        playerAntGoDir = if up then Forward else if down then Backward else Stop
    return
        w
            { wPlayerAnt =
                playerAnt
                    { antWheelPos = playerWheelPos,
                      antGoDir = playerAntGoDir
                    }
            }


updateAMWorld :: World -> World
updateAMWorld w =
    let playerAnt = wPlayerAnt w
        playerWheelPos = antWheelPos playerAnt
        playerAntGoDir = antGoDir playerAnt
        collisionRects = getCollisionRects w

        nextAngle =
            antAngle playerAnt
                & \angle ->
                    case playerWheelPos of
                        TurnRight -> (angle - antTurnAngle) `mod'` 360
                        TurnLeft -> (angle + antTurnAngle) `mod'` 360
                        Center -> angle `mod'` 360
        nextSpeed =
            case playerAntGoDir of
                Forward -> min antMaxSpeed (antSpeed playerAnt + antAcceleration)
                Backward -> max ((-antMaxSpeed) / 4) (antSpeed playerAnt - antAcceleration)
                Stop -> case compare (antSpeed playerAnt) 0 of
                    LT -> min 0 (antSpeed playerAnt + antAcceleration)
                    GT -> max 0 (antSpeed playerAnt - antAcceleration)
                    EQ -> 0
        nextPos = getNextPos nextAngle nextSpeed (antPos playerAnt)

        nextPos' =
            if checkCollisions nextPos collisionRects /= []
                then antPos playerAnt
                else nextPos

        playerAnt' =
            playerAnt
                { antPos = nextPos',
                  antAngle = nextAngle,
                  antSpeed = nextSpeed
                }
    in  w{wPlayerAnt = playerAnt'}


renderAMWorld :: World -> IO ()
renderAMWorld w = do
    let walls = zip (wWalls w) [red, green, blue] -- TODO Temporary
        playerAnt = wPlayerAnt w
        antPos' = antPos playerAnt

    -- draw walls
    -- TODO Why are you drawing the walls here?
    forM_ walls $ \(wall, color) -> drawRectangleRec wall color

    -- draw player ant as a circle
    drawCircleV antPos' 5 black

    -- draw ant direction as a line
    let antDir = getNextPos (antAngle playerAnt) 20 antPos'
    drawLineEx antPos' antDir 5 black


antMovementSys :: System World
antMovementSys = System handleAMInput updateAMWorld renderAMWorld


antMovementSysWrapped :: System World
antMovementSysWrapped =
    antMovementSys
        { render = \w -> drawing $ do
            f11Pressed <- isKeyPressed KeyF11
            when f11Pressed toggleFullscreen
            clearBackground lightGray
            renderAMWorld w
            -- drawFPS 10 10
        }


driveAntMovement :: IO ()
driveAntMovement =
    initAMWorld >>= gameLoop antMovementSysWrapped windowShouldClose
