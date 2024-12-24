{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE OverloadedRecordDot #-}

module FlatlandRenderer where

import Constants (
    antVisionAngle,
    antVisionMaxDistance,
    antVisionResolution,
    collisionRectSize,
    compassMaxDistance,
    foodColor,
    fps,
    initPheromoneAmount,
    nestColor,
    pheromoneColor,
    screenHeight,
    screenWidth,
    wallColor,
 )
import Control.Monad (forM_, when)
import Data.Fixed (mod')
import Data.Function ((&))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Shared (
    System (..),
    calcCenteredRect,
    calcRectCenter,
    gameLoop,
    getNextPos,
    isPointInRect,
    mkAnt,
    scalarTimesColor,
 )

import AntMovement (antMovementSys)
import Data.List (sortBy)
import Debug.Pretty.Simple (pTraceShowId, pTraceShowM)
import GHC.Float (int2Float)
import Raylib.Core (
    clearBackground,
    initWindow,
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

import Data.Foldable (Foldable (toList))
import Data.IntMap (update)
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
import System.Random (mkStdGen, randomIO)
import Types (
    Ant (..),
    Container (..),
    Degrees,
    EntityType (..),
    Food (..),
    GoDir (..),
    Nest (..),
    Pheromone (..),
    Sprite (..),
    VisionRay (..),
    WheelPos (..),
    World (..),
 )


-- Intersect a ray with a rectangle and return the distance to the intersection
intersectRayRect :: Vector2 -> Vector2 -> (Rectangle, EntityType) -> Maybe (Float, EntityType)
intersectRayRect
    camPos@(Vector2 rayOriginX rayOriginY)
    rayDir@(Vector2 rayDirX rayDirY)
    (rect@(Rectangle rectX rectY rectW rectH), entityType)
        -- Skip if the ant is inside the rectangle
        | isPointInRect camPos rect = Nothing
        | otherwise =
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
                    else Just (if distEntry < 0 then (distExit, entityType) else (distEntry, entityType))


-- Compute the minimum distance to any rectangle
minimumDistance :: Vector2 -> Vector2 -> [(Rectangle, EntityType)] -> Maybe (Float, EntityType)
minimumDistance camPos rayDir rects =
    let intersections = mapMaybe (intersectRayRect camPos rayDir) rects
        sortedIntersections = sortBy (\(d1, _) (d2, _) -> compare d1 d2) intersections
    in  listToMaybe sortedIntersections


-- Cast a ray and return the corresponding VisionRay
castRay :: Vector2 -> Float -> [(Rectangle, EntityType)] -> Float -> VisionRay
castRay camPos maxDist rects angle =
    let rad = (-angle) * deg2Rad
        rayDir = Vector2 (cos rad) (sin rad)
        minDist = minimumDistance camPos rayDir rects
        (dist, entityType) = fromMaybe (maxDist, UnknownET) minDist
        entityType' = if dist >= maxDist then UnknownET else entityType
        depth = 1 - normalize dist maxDist
        color = scalarTimesColor depth (entityTypeToColor entityType')
    in  VisionRay camPos angle (min dist maxDist) entityType' color


-- Calculate the vision rays for a given camera position and view parameters
calcVisionRays
    :: Vector2
    -> Float
    -> Float
    -> Int
    -> Float
    -> [(Rectangle, EntityType)]
    -> Seq VisionRay
calcVisionRays camPos camAngle camFov res maxDist rects =
    let halfFov = camFov / 2
        angleStep = camFov / int2Float (res - 1)
        anglesStart = camAngle + halfFov
        anglesNext = anglesStart - angleStep
        anglesEnd = camAngle - halfFov
        angles = [anglesStart, anglesNext .. anglesEnd]
        rays = Seq.fromList (map (castRay camPos maxDist rects) angles)
    in  rays


entityTypeToColor :: EntityType -> Color
entityTypeToColor et = case et of
    PlayerAntET -> blue
    AntET -> blue
    DeadAntET -> black
    PheromoneET -> pheromoneColor
    FoodET -> foodColor
    NestET -> nestColor
    WallET -> wallColor
    UnknownET -> black


makeVisionRect :: Int -> Int -> Int -> Rectangle
makeVisionRect x rectWidth rectHeight =
    Rectangle (int2Float x) 0 (int2Float rectWidth) (int2Float rectHeight)


-- Convert the vision rays to a row of tall rectangles for rendering.
visionRaysToRects :: Seq VisionRay -> [(Rectangle, Color)]
visionRaysToRects rays =
    let rectWidth = screenWidth `div` length rays
        rectHeight = screenHeight `div` 1
        colors = rays & fmap (.rColor) & toList
        rectsAndColors =
            zipWith
                (\x color -> (makeVisionRect x rectWidth rectHeight, color))
                [0, rectWidth ..]
                colors
    in  rectsAndColors


updateVisionRays :: [(Rectangle, EntityType)] -> Ant -> Ant
updateVisionRays rects ant =
    ant
        { aVisionRays =
            calcVisionRays
                ant.aPos
                ant.aAngle
                antVisionAngle
                antVisionResolution
                antVisionMaxDistance
                rects
        }


visionRayToLine :: VisionRay -> (Vector2, Vector2)
visionRayToLine (VisionRay p1 angle rayLength _ _) =
    (p1, getNextPos angle rayLength p1)


-- Normalize a value to the range [0, 1]
normalize :: Float -> Float -> Float
normalize x maxVal = min 1.0 (x / maxVal)


calcNestDirectionAndDistance :: Vector2 -> Vector2 -> (Degrees, Float)
calcNestDirectionAndDistance (Vector2 nestX nestY) (Vector2 antX antY) =
    let dx = nestX - antX
        dy = nestY - antY
        -- Normalize the angle to the range [0, 1]
        angle = ((-(atan2 dy dx * rad2Deg)) `mod'` 360) / 360
        -- Clamp and normalize the distance to the nest
        distance = normalize (sqrt (dx * dx + dy * dy)) compassMaxDistance
    in  (angle, distance)


initFRWorld :: IO World
initFRWorld = do
    let screenCenterW = int2Float screenWidth / 2
        screenCenterH = int2Float screenHeight / 2
        testWall1 = Rectangle 200 200 500 300
        testWall2 = Rectangle 100 300 1000 50
        walls = Seq.fromList [testWall1, testWall2]
    _ <- initWindow screenWidth screenHeight "Flatland Renderer"
    setTargetFPS fps
    setTraceLogLevel LogWarning
    setMouseCursor MouseCursorCrosshair
    seed <- randomIO
    let antPos = Vector2 screenCenterW screenCenterH
        playerAnt = mkAnt antPos seed
        nest = Nest (Container 0 (calcCenteredRect antPos collisionRectSize))
        pheromones =
            Seq.fromList
                [ Pheromone
                    ( Container
                        initPheromoneAmount
                        (calcCenteredRect (antPos |+| Vector2 100 100) collisionRectSize)
                    )
                ]
        food = Seq.fromList [Food (Container 10 (calcCenteredRect (antPos |+| Vector2 300 300) collisionRectSize))]
    return $ World playerAnt Seq.empty nest True True False True walls Nothing food Nothing pheromones


handleFRInput :: World -> IO World
handleFRInput w = do
    rKey <- isKeyPressed KeyR
    vKey <- isKeyPressed KeyV
    hKey <- isKeyPressed KeyH
    cKey <- isKeyPressed KeyC
    let toggleVisionRays = rKey /= w.wRenderVisionRays
        toggleVisionRects = vKey /= w.wRenderVisionRects
        toggleHomeVector = hKey /= w.wRenderHomeVector
        toggleHomeCompass = cKey /= w.wRenderHomeCompass
    return
        w
            { wRenderVisionRays = toggleVisionRays,
              wRenderVisionRects = toggleVisionRects,
              wRenderHomeVector = toggleHomeVector,
              wRenderHomeCompass = toggleHomeCompass
            }


collectVisibleRects :: World -> [(Rectangle, EntityType)]
collectVisibleRects w =
    let wallsRects = w.wWalls & fmap (\r -> (r, WallET)) & toList
        pheromoneRects = w.wPheromones & fmap (\p -> (p.pContainer.cRect, PheromoneET)) & toList
        foodRects = w.wFood & fmap (\f -> (f.fContainer.cRect, FoodET)) & toList
        nestRect = (w.wNest.nContainer.cRect, NestET)
    in  wallsRects ++ pheromoneRects ++ foodRects ++ [nestRect]


updateAntFR :: World -> Ant -> Ant
updateAntFR w ant =
    let visibleRects = w & collectVisibleRects
        nestPos = w.wNest.nContainer.cRect & calcRectCenter
        (nestAngle, nestDistance) = ant.aPos & calcNestDirectionAndDistance nestPos
    in  ant{aNestAngle = nestAngle, aNestDistance = nestDistance}
            & updateVisionRays visibleRects


updateFRWorld :: World -> World
updateFRWorld w = w{wPlayerAnt = updateAntFR w w.wPlayerAnt}


renderFRWorld :: World -> IO ()
renderFRWorld w = do
    let renderVisionRays = w.wRenderVisionRays
        renderVisionRects = w.wRenderVisionRects
        rays = w.wPlayerAnt.aVisionRays
        playerAnt = w.wPlayerAnt
        antPos = playerAnt.aPos

    -- TODO Drawing the walls is repeated in AntMovement.hs
    -- draw the nest
    -- drawCircleV nestPos 10 brown

    -- -- draw walls
    -- forM_ walls $ \(wall, color) -> drawRectangleRec wall color

    -- draw vision rays with colors
    when renderVisionRays $ do
        let visionLines = rays & fmap (\ray -> (visionRayToLine ray, ray.rColor))
        forM_ visionLines $ \((start, end), color) -> drawLineV start end color

    -- draw home vector for the player ant
    when w.wRenderHomeVector $ do
        let homeVectorEnd =
                getNextPos
                    (playerAnt.aNestAngle * 360)
                    (playerAnt.aNestDistance * compassMaxDistance * 0.2)
                    antPos
        drawLineEx antPos homeVectorEnd 5 gray

    -- TODO Drawing the ant is repeated in AntMovement.hs
    -- but I'm not sure how to avoid this duplication because I want the ant
    -- to be drawn on top of the vision rays.
    -- draw player ant as a circle
    drawCircleV antPos 5 black

    -- draw ant direction as a line
    let antDir = getNextPos playerAnt.aAngle 20 antPos
    drawLineEx antPos antDir 5 black

    -- draw ant vision rects
    when renderVisionRects $ do
        let visionRects = visionRaysToRects rays
        forM_ visionRects $ \(rect, color) -> drawRectangleRec rect color

    -- draw home compass in the lower right corner of the screen
    when w.wRenderHomeCompass $ do
        let compassCenter =
                Vector2 (int2Float screenWidth - 150) (int2Float screenHeight - 150)
            compassEnd =
                getNextPos
                    (playerAnt.aNestAngle * 360)
                    (playerAnt.aNestDistance * compassMaxDistance * 0.4)
                    compassCenter
            antDirEnd =
                getNextPos
                    playerAnt.aAngle
                    80
                    compassCenter

        -- If the ant has food, draw a piece of food in its mouth in the compass
        when playerAnt.aHasFood $ do
            drawCircleV antDirEnd 30 foodColor

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
                allSystems.render w
                -- drawFPS 10 10
            }


driveFlatlandRenderer :: IO ()
driveFlatlandRenderer =
    initFRWorld >>= gameLoop flatlandRendererSysWrapped windowShouldClose
