{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}

module FlatlandRenderer where

-- ------------------------------ PART Imports ------------------------------ --

import Shared (System (..), gameLoop)

import Control.Monad (forM_, when)
import Data.Fixed (mod')
import Data.Function ((&))
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe, mapMaybe)

-- import Debug.Trace (trace, traceShow)

import Constants (antPng, antStepSize, antVisionAngle, antVisionMaxDistance, antVisionResolution, borderWallThickness, fps, screenHeight, screenWidth, wallColor)
import Data.IntMap.Strict qualified as IntMap
import Debug.Trace (trace, traceShow)
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
import Raylib.Core.Shapes (drawCircleV, drawLineEx, drawLineV, drawRectangleRec)
import Raylib.Core.Text (drawFPS)
import Raylib.Core.Textures (drawTexturePro, drawTextureV, loadTexture, loadTextureFromImage)
import Raylib.Types (
    Color,
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
    Rectangle (Rectangle),
    Texture (texture'height, texture'width),
    TraceLogLevel (LogWarning),
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Types.Core.Textures (Image (..), PixelFormat (PixelFormatUncompressedGrayscale))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black, blue, green, lightGray, red, white)
import Raylib.Util.Math (deg2Rad, rad2Deg, (|+|))
import System.Random (mkStdGen, randomIO, randomR)
import Types (Ant (..), Circle (..), Entity (..), Mode (..), Sprite (..), VisionRay (..), WheelPos (..), World (..))


-- ------------------------- PART Flatland Renderer ------------------------- --

-- Intersect a ray with a rectangle and return the distance to the intersection
intersectRayRect :: Vector2 -> Vector2 -> Rectangle -> Maybe Float
intersectRayRect
    (Vector2 rayOriginX rayOriginY)
    (Vector2 rayDirX rayDirY)
    (Rectangle rectX rectY rectW rectH) =
        let
            -- Intersection distances for the vertical edges of the rectangle
            distNearX = (rectX - rayOriginX) / rayDirX
            distFarX = (rectX + rectW - rayOriginX) / rayDirX

            -- Intersection distances for the horizontal edges of the rectangle
            distNearY = (rectY - rayOriginY) / rayDirY
            distFarY = (rectY + rectH - rayOriginY) / rayDirY

            -- Calculate the entry and exit distances along the ray
            distEntry = max (min distNearX distFarX) (min distNearY distFarY)
            distExit = min (max distNearX distFarX) (max distNearY distFarY)
        in
            -- Determine if there is an intersection
            if distExit < 0 || distEntry > distExit
                then Nothing
                else Just (if distEntry < 0 then distExit else distEntry)


calcVisionRays :: Vector2 -> Float -> Float -> Int -> Float -> [Rectangle] -> [VisionRay]
calcVisionRays camPos camAngle camFov res maxDist rects =
    let halfFov = camFov / 2
        angleStep = camFov / int2Float (res - 1)
        anglesStart = camAngle - halfFov
        anglesNext = anglesStart + angleStep
        anglesEnd = camAngle + halfFov
        angles = [anglesStart, anglesNext .. anglesEnd]
        rays = map castRay angles
    in  rays
    where
        castRay :: Float -> VisionRay
        castRay angle =
            let rad = (-angle) * deg2Rad
                rayDir = Vector2 (cos rad) (sin rad)
                dist = fromMaybe maxDist $ minimumDistance camPos rayDir rects
            in  VisionRay camPos angle (min dist maxDist)


-- Normalize the distance based on max distance
normalizeDistance :: Float -> Float
normalizeDistance dist = min 1.0 (dist / antVisionMaxDistance)


-- Compute the minimum distance to any rectangle
minimumDistance :: Vector2 -> Vector2 -> [Rectangle] -> Maybe Float
minimumDistance camPos rayDir rects =
    let intersections = mapMaybe (intersectRayRect camPos rayDir) rects
    in  if null intersections then Nothing else Just (minimum intersections)


depthMap2Image :: Int -> [Float] -> Image
depthMap2Image height depthMap =
    let width = length depthMap
        gamma = 0.4545
        pixels = concat $ replicate height $ map (round . (* 255) . (** gamma) . (1 -)) depthMap
    in  Image pixels width height 1 PixelFormatUncompressedGrayscale


renderPlayerAntVision :: Int -> Ant -> Image
renderPlayerAntVision height ant =
    let depthMap = antVisionRays ant & map (normalizeDistance . rayLength)
    in  depthMap2Image height depthMap


-- --------------------------------- Vision --------------------------------- --

updateVisionRays :: [Rectangle] -> Ant -> Ant
updateVisionRays walls ant =
    let visionRays = calcVisionRays (antPos ant) (antAngle ant) antVisionAngle antVisionResolution antVisionMaxDistance walls
    in  ant{antVisionRays = visionRays}


-- ---------------------------- PART Constructors --------------------------- --

mkPlayerAnt :: Float -> Float -> Int -> Ant
mkPlayerAnt x y seed =
    let rng = mkStdGen seed
    in  Ant (Vector2 x y) 0 0 SeekFood rng False Center LeftSprite []


-- ------------------------------- PART Utils ------------------------------- --

getNextPos :: Float -> Float -> Float -> Vector2 -> Vector2
getNextPos angle speed stepSize (Vector2 x y) =
    let rad = (-angle) * deg2Rad -- negate angle because of screen space coords
        x' = x + stepSize * speed * cos rad
        y' = y + stepSize * speed * sin rad
    in  Vector2 x' y'


visionRayToLine :: VisionRay -> (Vector2, Vector2)
visionRayToLine (VisionRay p1 angle rayLength) =
    (p1, getNextPos angle 1 rayLength p1)


-- ----------------------------- PART Game Loop ----------------------------- --

initFRWorld :: IO World
initFRWorld = do
    let screenCenterW = int2Float screenWidth / 2
        screenCenterH = int2Float screenHeight / 2
        testWall1 = Rectangle 200 200 500 300
        testWall2 = Rectangle 100 300 1000 50
        testWall3 = Rectangle 500 600 50 50
        walls = [testWall1, testWall2, testWall3]
    window <- initWindow screenWidth screenHeight "Flatland Renderer"
    setTargetFPS fps
    setTraceLogLevel LogWarning
    setMouseCursor MouseCursorCrosshair
    antTexture <- loadTexture antPng window
    let rng = mkStdGen 0
        playerAnt = Ant (Vector2 screenCenterW screenCenterH) 0 0 SeekFood rng False Center LeftSprite []
    return $ World window antTexture playerAnt True walls Nothing


handleFRInput :: World -> IO World
handleFRInput w = do
    up <- isKeyDown KeyUp
    left <- isKeyDown KeyLeft
    right <- isKeyDown KeyRight
    vKey <- isKeyPressed KeyV
    let toggleVisionRays = vKey /= wRenderVisionRays w
        playerAnt = wPlayerAnt w
        playerWheelPos =
            antWheelPos playerAnt
                & \_ ->
                    if right then TurnRight else if left then TurnLeft else Center
        playerAntGo = up
    return
        w
            { wRenderVisionRays = toggleVisionRays,
              wPlayerAnt =
                (wPlayerAnt w)
                    { antWheelPos = playerWheelPos,
                      antGo = playerAntGo
                    }
            }


updateFRWorld :: World -> World
updateFRWorld w =
    let playerAnt = wPlayerAnt w
        playerWheelPos = antWheelPos playerAnt
        playerAntGo = antGo playerAnt
        playerAntAngle =
            antAngle playerAnt
                & \angle ->
                    case playerWheelPos of
                        TurnRight -> angle - 5
                        TurnLeft -> angle + 5
                        Center -> angle
                        & \angle' -> angle' `mod'` 360
        nextPos =
            if playerAntGo
                then getNextPos playerAntAngle 1 5 (antPos playerAnt)
                else antPos playerAnt
        playerAnt' = playerAnt{antPos = nextPos, antAngle = playerAntAngle}
        playerAnt'' = updateVisionRays (wWalls w) playerAnt'
    in  w{wPlayerAnt = playerAnt''}


renderFRWorld :: World -> IO ()
renderFRWorld w = do
    f11Pressed <- isKeyPressed KeyF11
    when f11Pressed toggleFullscreen
    let walls = wWalls w
        renderVisionRays = wRenderVisionRays w
        rays = wPlayerAnt w & antVisionRays
        playerAnt = wPlayerAnt w
    drawing $ do
        clearBackground lightGray
        forM_ walls $ \wall -> drawRectangleRec wall wallColor
        when renderVisionRays $ do
            let visionLines = map visionRayToLine rays
            forM_ visionLines $ \(start, end) -> drawLineV start end green
        drawCircleV (antPos playerAnt) 5 black
        -- draw ant direction as a line
        let antDir = getNextPos (antAngle playerAnt) 1 20 (antPos playerAnt)
        drawLineEx (antPos playerAnt) antDir 5 black
        drawFPS 10 10


flatlandRendererSys :: System World
flatlandRendererSys = System handleFRInput updateFRWorld renderFRWorld


driveFlatlandRenderer :: IO ()
driveFlatlandRenderer =
    initFRWorld >>= gameLoop flatlandRendererSys windowShouldClose
