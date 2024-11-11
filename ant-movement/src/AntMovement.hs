{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE OverloadedRecordDot #-}

module AntMovement where

import Constants (
    antAcceleration,
    antMaxSpeed,
    antTurnAngle,
    collisionRectSize,
    fps,
    screenHeight,
    screenWidth,
 )
import Control.Monad (forM_, when)
import Data.Fixed (mod')
import Data.Function ((&))
import Data.IntMap (update)
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
import Raylib.Types (
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
    Rectangle (Rectangle),
    TraceLogLevel (LogWarning),
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black, blue, brown, green, lightGray, red)
import Shared (System (..), calcCenteredRect, gameLoop, getNextPos, mkAnt)
import System.Random (mkStdGen, randomIO)
import Types (
    Ant (..),
    Container (..),
    EntityType (..),
    GoDir (..),
    Nest (..),
    Sprite (..),
    WheelPos (..),
    World (..),
 )


canGoThere :: Vector2 -> (Rectangle, EntityType) -> Maybe (Rectangle, EntityType)
canGoThere (Vector2 x y) (rect@(Rectangle rx ry rw rh), entityType) =
    if x < rx || x > rx + rw || y < ry || y > ry + rh
        then Nothing
        else Just (rect, entityType)


checkCollisions :: Vector2 -> [(Rectangle, EntityType)] -> [(Rectangle, EntityType)]
checkCollisions pos rects = mapMaybe (canGoThere pos) rects


getCollisionRects :: World -> [(Rectangle, EntityType)]
getCollisionRects w =
    let walls = map (\wall -> (wall, WallET)) w.wWalls
        foods = [] -- map (\food -> (foodCollisionRect food, FoodET)) (wFood w)
    in  walls ++ foods


wrapAroundScreen :: Vector2 -> Vector2
wrapAroundScreen (Vector2 x y) =
    let x' = x `mod'` int2Float screenWidth
        y' = y `mod'` int2Float screenHeight
    in  Vector2 x' y'


initAMWorld :: IO World
initAMWorld = do
    let testWall1 = Rectangle 200 200 500 300
        testWall2 = Rectangle 100 300 1000 50
        testWall3 = Rectangle 900 500 20 20
        walls = [testWall1, testWall2, testWall3]
    _ <- initWindow screenWidth screenHeight "Ant Movement"
    setTargetFPS fps
    setTraceLogLevel LogWarning
    setMouseCursor MouseCursorCrosshair
    seed <- randomIO
    let antPos' = Vector2 (int2Float screenWidth / 2) (int2Float screenHeight / 2)
        playerAnt = mkAnt antPos' seed
        nest = Nest (Container 0 (calcCenteredRect antPos' collisionRectSize))
    return $ World playerAnt [] nest True True False True walls Nothing [] Nothing []


handleAMInput :: World -> IO World
handleAMInput w = do
    up <- isKeyDown KeyUp
    down <- isKeyDown KeyDown
    left <- isKeyDown KeyLeft
    right <- isKeyDown KeyRight
    let playerAnt = w.wPlayerAnt
        playerWheelPos =
            playerAnt.aWheelPos
                & \_ ->
                    if right then TurnRight else if left then TurnLeft else Center
        playerAntGoDir = if up then Forward else if down then Backward else Stop
    return
        w
            { wPlayerAnt =
                playerAnt
                    { aWheelPos = playerWheelPos,
                      aGoDir = playerAntGoDir
                    }
            }


updateAntMovement :: World -> Ant -> Ant
updateAntMovement w ant =
    let collisionRects = getCollisionRects w
        nextAngle =
            ant.aAngle
                & \angle ->
                    case ant.aWheelPos of
                        TurnRight -> (angle - antTurnAngle) `mod'` 360
                        TurnLeft -> (angle + antTurnAngle) `mod'` 360
                        Center -> angle `mod'` 360
        nextSpeed =
            case ant.aGoDir of
                Forward -> min antMaxSpeed (ant.aSpeed + antAcceleration)
                Backward -> max ((-antMaxSpeed) / 4) (ant.aSpeed - antAcceleration)
                Stop -> case compare ant.aSpeed 0 of
                    LT -> min 0 (ant.aSpeed + antAcceleration)
                    GT -> max 0 (ant.aSpeed - antAcceleration)
                    EQ -> 0
        nextPos = getNextPos nextAngle nextSpeed ant.aPos

        nextPos' =
            if checkCollisions nextPos collisionRects /= []
                then ant.aPos
                else nextPos

        ant' =
            ant
                { aPos = wrapAroundScreen nextPos',
                  aAngle = nextAngle,
                  aSpeed = nextSpeed
                }
    in  ant'


updateAMWorld :: World -> World
updateAMWorld w = w{wPlayerAnt = updateAntMovement w w.wPlayerAnt}


renderAMWorld :: World -> IO ()
renderAMWorld w = do
    let walls = zip w.wWalls [red, green, blue] -- TODO Temporary
        playerAnt = w.wPlayerAnt
        antPos' = playerAnt.aPos

    -- draw walls
    -- TODO Why are you drawing the walls here?
    -- forM_ walls $ \(wall, color) -> drawRectangleRec wall color

    -- draw player ant as a circle
    drawCircleV antPos' 5 black

    -- draw ant direction as a line
    let antDir = getNextPos playerAnt.aAngle 20 antPos'
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
