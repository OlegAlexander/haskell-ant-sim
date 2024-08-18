{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use tuple-section" #-}

module FlatlandRenderer where

import Constants (
    antPng,
    antVisionAngle,
    antVisionMaxDistance,
    antVisionResolution,
    fps,
    screenHeight,
    screenWidth,
    wallColor,
 )
import Control.Monad (forM_, when)
import Data.Fixed (mod')
import Data.Function ((&))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Shared (System (..), gameLoop)

-- import Debug.Trace (traceShowId)

import Data.List (sortBy)
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
    drawLineV,
    drawRectangleRec,
 )
import Raylib.Core.Text (drawFPS)
import Raylib.Core.Textures (loadTexture)
import Raylib.Types (
    Color (..),
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
    Rectangle (Rectangle),
    TraceLogLevel (LogWarning),
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black, blue, gray, green, lightGray, red, white)
import Raylib.Util.Math (deg2Rad)
import System.Random (mkStdGen)
import Types (
    Ant (..),
    EntityType (..),
    Mode (..),
    Sprite (..),
    VisionRay (..),
    WheelPos (..),
    World (..),
 )


-- Intersect a ray with a rectangle and return the distance to the intersection
intersectRayRect
    :: Vector2
    -> Vector2
    -> (Rectangle, EntityType)
    -> Maybe (Float, EntityType)
intersectRayRect
    (Vector2 rayOriginX rayOriginY)
    (Vector2 rayDirX rayDirY)
    (Rectangle rectX rectY rectW rectH, entityType) =
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
                else
                    Just
                        ( if distEntry < 0
                            then (distExit, entityType)
                            else (distEntry, entityType)
                        )


-- Compute the minimum distance to any rectangle
minimumDistance
    :: Vector2
    -> Vector2
    -> [(Rectangle, EntityType)]
    -> Maybe (Float, EntityType)
minimumDistance camPos rayDir rects =
    let intersections = mapMaybe (intersectRayRect camPos rayDir) rects
        sortedIntersections =
            sortBy (\(d1, _) (d2, _) -> compare d1 d2) intersections
    in  listToMaybe sortedIntersections


-- Cast a ray and return the corresponding VisionRay
castRay :: Vector2 -> Float -> [(Rectangle, EntityType)] -> Float -> VisionRay
castRay camPos maxDist rects angle =
    let rad = (-angle) * deg2Rad
        rayDir = Vector2 (cos rad) (sin rad)
        minDist = minimumDistance camPos rayDir rects
        (dist, entityType) = fromMaybe (maxDist, UnknownET) minDist
        entityType' = if dist >= maxDist then UnknownET else entityType
    in  VisionRay camPos angle (min dist maxDist) entityType'


-- Calculate the vision rays for a given camera position and view parameters
calcVisionRays
    :: Vector2
    -> Float
    -> Float
    -> Int
    -> Float
    -> [(Rectangle, EntityType)]
    -> [VisionRay]
calcVisionRays camPos camAngle camFov res maxDist rects =
    let halfFov = camFov / 2
        angleStep = camFov / int2Float (res - 1)
        anglesStart = camAngle + halfFov
        anglesNext = anglesStart - angleStep
        anglesEnd = camAngle - halfFov
        angles = [anglesStart, anglesNext .. anglesEnd]
        rays = map (castRay camPos maxDist rects) angles
    in  rays


-- Normalize the distance based on max distance
normalizeDistance :: Float -> Float
normalizeDistance dist = min 1.0 (dist / antVisionMaxDistance)


entityTypeToColor :: EntityType -> Color
entityTypeToColor = \case
    PlayerAntET -> blue
    AntET -> blue
    DeadAntET -> white
    PheromoneET -> green
    FoodET -> Color 255 165 0 255
    NestET -> Color 255 255 0 255
    WallET -> red
    UnknownET -> black


rgbToLinear :: Color -> (Float, Float, Float, Float)
rgbToLinear (Color r g b a) =
    let r' = fromIntegral r / 255
        g' = fromIntegral g / 255
        b' = fromIntegral b / 255
        a' = fromIntegral a / 255
    in  (r', g', b', a')


linearToRgb :: (Float, Float, Float, Float) -> Color
linearToRgb (r, g, b, a) =
    let r' = round $ r * 255
        g' = round $ g * 255
        b' = round $ b * 255
        a' = round $ a * 255
    in  Color r' g' b' a'


scalarTimesColor :: Float -> Color -> Color
scalarTimesColor scalar color =
    let (r', g', b', a') = rgbToLinear color
    in  linearToRgb (r' * scalar, g' * scalar, b' * scalar, a')


-- Convert the vision rays to a row of tall rectangles for rendering.
visionRaysToRects :: [VisionRay] -> [(Rectangle, Color)]
visionRaysToRects rays =
    let depthMap = map ((1 -) . normalizeDistance . rayLength) rays
        rectWidth = screenWidth `div` length depthMap
        rectHeight = screenHeight `div` 4
        colors = map (entityTypeToColor . rayHitEntityType) rays
        colorsTimesDepthMap = zipWith scalarTimesColor depthMap colors
        rectsAndColors =
            zipWith
                ( \x color ->
                    ( Rectangle
                        (int2Float x)
                        0
                        (int2Float rectWidth)
                        (int2Float rectHeight),
                      color
                    )
                )
                [0, rectWidth ..]
                colorsTimesDepthMap
    in  rectsAndColors


updateVisionRays :: [(Rectangle, EntityType)] -> Ant -> Ant
updateVisionRays rects ant =
    let visionRays =
            calcVisionRays
                (antPos ant)
                (antAngle ant)
                antVisionAngle
                antVisionResolution
                antVisionMaxDistance
                rects
    in  ant{antVisionRays = visionRays}


mkPlayerAnt :: Float -> Float -> Int -> Ant
mkPlayerAnt x y seed =
    let rng = mkStdGen seed
    in  Ant (Vector2 x y) 0 0 SeekFood rng False Center LeftSprite []


getNextPos :: Float -> Float -> Float -> Vector2 -> Vector2
getNextPos angle speed stepSize (Vector2 x y) =
    let rad = (-angle) * deg2Rad -- negate angle because of screen space coords
        x' = x + stepSize * speed * cos rad
        y' = y + stepSize * speed * sin rad
    in  Vector2 x' y'


visionRayToLine :: VisionRay -> (Vector2, Vector2)
visionRayToLine (VisionRay p1 angle rayLength _) =
    (p1, getNextPos angle 1 rayLength p1)


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
        antPos = Vector2 screenCenterW screenCenterH
        playerAnt = Ant antPos 0 0 SeekFood rng False Center LeftSprite []
    return $ World window antTexture playerAnt True True walls Nothing


handleFRInput :: World -> IO World
handleFRInput w = do
    up <- isKeyDown KeyUp
    left <- isKeyDown KeyLeft
    right <- isKeyDown KeyRight
    rKey <- isKeyPressed KeyR
    vKey <- isKeyPressed KeyV
    let toggleVisionRays = rKey /= wRenderVisionRays w
        toggleVisionRects = vKey /= wRenderVisionRects w
        playerAnt = wPlayerAnt w
        playerWheelPos =
            antWheelPos playerAnt
                & \_ ->
                    if right then TurnRight else if left then TurnLeft else Center
        playerAntGo = up
    return
        w
            { wRenderVisionRays = toggleVisionRays,
              wRenderVisionRects = toggleVisionRects,
              wPlayerAnt =
                playerAnt
                    { antWheelPos = playerWheelPos,
                      antGo = playerAntGo
                    }
            }


updateFRWorld :: World -> World
updateFRWorld w =
    let playerAnt = wPlayerAnt w
        playerWheelPos = antWheelPos playerAnt
        playerAntGo = antGo playerAnt
        wallRects = zip (wWalls w) [WallET, PheromoneET, AntET]
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
        playerAnt'' = updateVisionRays wallRects playerAnt'
    in  w{wPlayerAnt = playerAnt''}


renderFRWorld :: World -> IO ()
renderFRWorld w = do
    let walls = zip (wWalls w) [red, green, blue] -- TODO Temporary
        renderVisionRays = wRenderVisionRays w
        renderVisionRects = wRenderVisionRects w
        rays = wPlayerAnt w & antVisionRays
        playerAnt = wPlayerAnt w
    forM_ walls $ \(wall, color) -> drawRectangleRec wall color
    when renderVisionRays $ do
        let visionLines = map visionRayToLine rays
        forM_ visionLines $ \(start, end) -> drawLineV start end white
    drawCircleV (antPos playerAnt) 5 black
    -- draw ant direction as a line
    let antDir = getNextPos (antAngle playerAnt) 1 20 (antPos playerAnt)
    drawLineEx (antPos playerAnt) antDir 5 black
    -- draw ant vision rects
    when renderVisionRects $ do
        let visionRects = visionRaysToRects rays
        forM_ visionRects $ \(rect, color) -> drawRectangleRec rect color


flatlandRendererSys :: System World
flatlandRendererSys = System handleFRInput updateFRWorld renderFRWorld


flatlandRendererSysWrapped :: System World
flatlandRendererSysWrapped =
    flatlandRendererSys
        { render = \w -> drawing $ do
            f11Pressed <- isKeyPressed KeyF11
            when f11Pressed toggleFullscreen
            clearBackground lightGray
            renderFRWorld w
            -- drawFPS 10 10
        }


driveFlatlandRenderer :: IO ()
driveFlatlandRenderer =
    initFRWorld >>= gameLoop flatlandRendererSysWrapped windowShouldClose
