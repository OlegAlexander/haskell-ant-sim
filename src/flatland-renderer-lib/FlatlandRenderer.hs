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
import Shared (System (..), gameLoop, getNextPos)

-- import Debug.Trace (traceShowId)

import AntMovement (antMovementSys)
import Data.List (sortBy)
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
import Raylib.Util.Colors (black, blue, brown, gray, green, lightGray, red, white)
import Raylib.Util.Math (Vector (..), deg2Rad, rad2Deg)
import System.Random (mkStdGen)
import Text.Read.Lex qualified as AntMovement
import Types (Ant (..), Degrees, EntityType (..), GoDir (..), Mode (..), Sprite (..), VisionRay (..), WheelPos (..), World (..))


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
        rectHeight = screenHeight `div` 1
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
    in  Ant (Vector2 x y) 0 0 SeekFood rng Stop Center LeftSprite [] 0 0 False


visionRayToLine :: VisionRay -> (Vector2, Vector2)
visionRayToLine (VisionRay p1 angle rayLength _) =
    (p1, getNextPos angle rayLength p1)


calcNestDirectionAndDistance :: Vector2 -> Vector2 -> (Degrees, Float)
calcNestDirectionAndDistance (Vector2 nestX nestY) (Vector2 antX antY) =
    let dx = nestX - antX
        dy = nestY - antY
        angle = (-(atan2 dy dx * rad2Deg)) `mod'` 360
        distance = sqrt (dx * dx + dy * dy)
    in  (angle, distance)


initFRWorld :: IO World
initFRWorld = do
    let screenCenterW = int2Float screenWidth / 2
        screenCenterH = int2Float screenHeight / 2
        testWall1 = Rectangle 200 200 500 300
        testWall2 = Rectangle 100 300 1000 50
        testWall3 = Rectangle 900 500 20 20
        walls = [testWall1, testWall2, testWall3]
    window <- initWindow screenWidth screenHeight "Flatland Renderer"
    setTargetFPS fps
    setTraceLogLevel LogWarning
    setMouseCursor MouseCursorCrosshair
    antTexture <- loadTexture antPng window
    let rng = mkStdGen 0
        antPos = Vector2 screenCenterW screenCenterH
        nestPos = antPos
        playerAnt = Ant antPos 0 0 SeekFood rng Stop Center LeftSprite [] 0 0 False
    return $ World window antTexture playerAnt nestPos True True False True walls Nothing [] Nothing


handleFRInput :: World -> IO World
handleFRInput w = do
    rKey <- isKeyPressed KeyR
    vKey <- isKeyPressed KeyV
    hKey <- isKeyPressed KeyH
    cKey <- isKeyPressed KeyC
    let toggleVisionRays = rKey /= wRenderVisionRays w
        toggleVisionRects = vKey /= wRenderVisionRects w
        toggleHomeVector = hKey /= wRenderHomeVector w
        toggleHomeCompass = cKey /= wRenderHomeCompass w
    return
        w
            { wRenderVisionRays = toggleVisionRays,
              wRenderVisionRects = toggleVisionRects,
              wRenderHomeVector = toggleHomeVector,
              wRenderHomeCompass = toggleHomeCompass
            }


updateFRWorld :: World -> World
updateFRWorld w =
    let playerAnt = wPlayerAnt w
        wallRects = zip (wWalls w) [WallET, PheromoneET, AntET]
        (nestAngle, nestDistance) = calcNestDirectionAndDistance (wNest w) (antPos playerAnt)
        playerAnt' =
            playerAnt
                { antNestAngle = nestAngle,
                  antNestDistance = nestDistance
                }
        playerAnt'' = updateVisionRays wallRects playerAnt'
    in  w{wPlayerAnt = playerAnt''}


renderFRWorld :: World -> IO ()
renderFRWorld w = do
    let walls = zip (wWalls w) [red, green, blue] -- TODO Temporary
        renderVisionRays = wRenderVisionRays w
        renderVisionRects = wRenderVisionRects w
        rays = wPlayerAnt w & antVisionRays
        playerAnt = wPlayerAnt w
        antPos' = antPos playerAnt

    -- TODO Drawing the walls is repeated in AntMovement.hs
    -- draw the nest
    drawCircleV (wNest w) 10 brown

    -- draw walls
    forM_ walls $ \(wall, color) -> drawRectangleRec wall color

    -- draw vision rays
    when renderVisionRays $ do
        let visionLines = map visionRayToLine rays
        forM_ visionLines $ \(start, end) -> drawLineV start end white

    -- draw home vector for the player ant
    when (wRenderHomeVector w) $ do
        let homeVectorEnd =
                getNextPos
                    (antNestAngle playerAnt)
                    (antNestDistance playerAnt * 0.2)
                    antPos'
        drawLineEx antPos' homeVectorEnd 5 gray

    -- TODO Drawing the ant is repeated in AntMovement.hs
    -- but I'm not sure how to avoid this duplication because I want the ant
    -- to be drawn on top of the vision rays.
    -- draw player ant as a circle
    drawCircleV antPos' 5 black

    -- draw ant direction as a line
    let antDir = getNextPos (antAngle playerAnt) 20 antPos'
    drawLineEx antPos' antDir 5 black

    -- draw ant vision rects
    when renderVisionRects $ do
        let visionRects = visionRaysToRects rays
        forM_ visionRects $ \(rect, color) -> drawRectangleRec rect color

    -- draw home compass in the lower right corner of the screen
    when (wRenderHomeCompass w) $ do
        let compassCenter =
                Vector2 (int2Float screenWidth - 150) (int2Float screenHeight - 150)
            compassEnd =
                getNextPos
                    (antNestAngle playerAnt)
                    (antNestDistance playerAnt * 0.4)
                    compassCenter
            antDirEnd =
                getNextPos
                    (antAngle playerAnt)
                    80
                    compassCenter
        drawCircleV compassCenter 20 gray
        drawLineEx compassCenter antDirEnd 20 gray
        drawLineEx compassCenter compassEnd 10 white


flatlandRendererSys :: System World
flatlandRendererSys = System handleFRInput updateFRWorld renderFRWorld


flatlandRendererSysWrapped :: System World
flatlandRendererSysWrapped =
    let allSystems = antMovementSys <> flatlandRendererSys
    in  allSystems
            { render = \w -> drawing $ do
                f11Pressed <- isKeyPressed KeyF11
                when f11Pressed toggleFullscreen
                clearBackground lightGray
                render allSystems w
                -- drawFPS 10 10
            }


driveFlatlandRenderer :: IO ()
driveFlatlandRenderer =
    initFRWorld >>= gameLoop flatlandRendererSysWrapped windowShouldClose
