{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE OverloadedRecordDot #-}

module AntMovement where

import Constants (antAcceleration, antJitterAngle, antTurnAngle, bgColor, fps, screenHeight, screenWidth, wallColor)
import Control.Monad (forM_, when)
import Data.Fixed (mod')
import Data.Foldable (Foldable (toList))
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Sequence qualified as Seq
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
import Raylib.Core.Shapes (
    drawCircleV,
    drawLineEx,
    drawRectangleRec,
 )
import Raylib.Types (
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
    Rectangle (Rectangle),
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black, lightGray)
import Shared (System (..), defaultWorld, gameLoop, getNextPos)
import System.Random (StdGen, newStdGen, randomR)
import Types (
    Ant (..),
    EntityType (..),
    GoDir (..),
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
    let walls = w.wWalls & fmap (\wall -> (wall, WallET)) & toList
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
        walls = Seq.fromList [testWall1, testWall2, testWall3]
    _ <- initWindow screenWidth screenHeight "Ant Movement"
    setTargetFPS fps
    setMouseCursor MouseCursorCrosshair
    rng <- newStdGen
    return (defaultWorld rng){wWalls = walls}


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


getNextAngle :: StdGen -> Ant -> (Float, StdGen)
getNextAngle rng ant =
    let (jitter, rng') = rng & randomR (-antJitterAngle, antJitterAngle)
        jitterAmount = jitter * min (abs ant.aSpeed) 1
        angle = case ant.aWheelPos of
            TurnRight -> ant.aAngle - antTurnAngle + jitterAmount
            TurnLeft -> ant.aAngle + antTurnAngle + jitterAmount
            Center -> ant.aAngle + jitterAmount
    in  (angle `mod'` 360, rng')


getNextSpeed :: Ant -> Float
getNextSpeed ant =
    case ant.aGoDir of
        Forward -> min ant.aMaxSpeed (ant.aSpeed + antAcceleration)
        Backward -> max ((-ant.aMaxSpeed) * 0.33) (ant.aSpeed - antAcceleration)
        Stop -> case compare ant.aSpeed 0 of
            LT -> min 0 (ant.aSpeed + antAcceleration)
            GT -> max 0 (ant.aSpeed - antAcceleration)
            EQ -> 0


updateAntMovement :: [(Rectangle, EntityType)] -> StdGen -> Ant -> (Ant, StdGen)
updateAntMovement collisionRects rng ant =
    let (nextAngle, rng') = getNextAngle rng ant
        nextSpeed = getNextSpeed ant
        nextPos = getNextPos nextAngle nextSpeed ant.aPos
        nextPos' =
            if checkCollisions nextPos collisionRects /= []
                then ant.aPos
                else nextPos
    in  ( ant
            { aPos = wrapAroundScreen nextPos',
              aAngle = nextAngle,
              aSpeed = nextSpeed
            },
          rng'
        )


updateAMWorld :: World -> World
updateAMWorld w =
    let collisionRects = getCollisionRects w
        (wPlayerAnt, rng) = updateAntMovement collisionRects w.wRng w.wPlayerAnt
    in  w{wPlayerAnt = wPlayerAnt, wRng = rng}


renderAMWorld :: World -> IO ()
renderAMWorld w = do
    let walls = w.wWalls & toList
        playerAnt = w.wPlayerAnt
        antPos = playerAnt.aPos

    -- draw walls
    forM_ walls $ \wall -> drawRectangleRec wall wallColor

    -- draw player ant as a circle
    drawCircleV antPos 5 black

    -- draw ant direction as a line
    let antDir = antPos & getNextPos playerAnt.aAngle 20
    drawLineEx antPos antDir 5 black


antMovementSys :: System World
antMovementSys = System handleAMInput updateAMWorld renderAMWorld


antMovementSysWrapped :: System World
antMovementSysWrapped =
    antMovementSys
        { render = \w -> drawing $ do
            f11Pressed <- isKeyPressed KeyF11
            when f11Pressed toggleFullscreen
            clearBackground bgColor
            renderAMWorld w
            -- drawFPS 10 10
        }


driveAntMovement :: IO ()
driveAntMovement =
    initAMWorld >>= gameLoop antMovementSysWrapped windowShouldClose
