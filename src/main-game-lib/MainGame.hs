{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}

module MainGame where

-- ------------------------------ PART Imports ------------------------------ --

import Control.Monad (forM_, when)
import Data.Fixed (mod')
import Data.Function ((&))
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe, mapMaybe)

-- import Debug.Trace (trace, traceShow)

import Constants (
    antAcceleration,
    antDeceleration,
    antJitterAngle,
    antMaxSpeed,
    antPng,
    antScale,
    antStepSize,
    antTurnAngle,
    antVisionAngle,
    antVisionMaxDistance,
    antVisionResolution,
    borderWallThickness,
    fps,
    screenHeight,
    screenWidth,
    title,
    wallColor,
 )
import Data.IntMap.Strict qualified as IntMap
import DrawWalls (drawWallsSys1, handleWallInput, renderWallsWorld, updateWallsWorld)
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
import Raylib.Core.Shapes (drawCircleV, drawLineV, drawRectangleRec)
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
import Raylib.Util.Colors (blue, green, lightGray, red, white)
import Raylib.Util.Math (deg2Rad, rad2Deg)
import Shared (System (..), gameLoop)
import System.Random (mkStdGen, randomIO, randomR)
import Types (
    Ant (..),
    Circle (Circle),
    Entity (..),
    Mode (SeekFood),
    Sprite (LeftSprite, RightSprite),
    VisionRay (VisionRay, rayLength),
    WheelPos (Center, TurnLeft, TurnRight),
    World (..),
 )


-- ----------------------------- PART Constants ----------------------------- --

-- TODO Consider using 4 different colors for the walls to orient oneself.
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
            let rad = angle * deg2Rad
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


renderAntVision :: Int -> Ant -> Image
renderAntVision height ant =
    let depthMap = antVisionRays ant & map (normalizeDistance . rayLength)
    in  depthMap2Image height depthMap


-- ----------------------------- PART Player Ant ---------------------------- --

mkAnt :: Float -> Float -> Int -> Ant
mkAnt x y seed =
    let (angle, rng) = randomR (0, 360) (mkStdGen seed)
    in  Ant (Vector2 x y) angle 0 SeekFood rng False Center LeftSprite []


mkAnts :: Float -> Float -> [Int] -> [Ant]
mkAnts x y seeds = map (mkAnt x y) seeds


printAnts :: [Entity] -> IO ()
printAnts = putStrLn . unlines . map show


stepAnt :: Vector2 -> Ant -> Ant
stepAnt nextPos ant = ant{antPos = nextPos}


getNextAntPos :: Ant -> Vector2
getNextAntPos ant =
    let angle = antAngle ant * deg2Rad
        speed = antSpeed ant
        Vector2 x y = antPos ant
        x' = x + antStepSize * speed * cos angle
        y' = y + antStepSize * speed * sin angle
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
            if antGo rotatedAnt
                then goAnt rotatedAnt
                else stopAnt rotatedAnt
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
    let speed' = max 0 (antSpeed ant - antDeceleration)
    in  ant{antSpeed = speed'}


leftAnt :: Ant -> Ant
leftAnt ant = rotateAnt (-antTurnAngle) ant


rightAnt :: Ant -> Ant
rightAnt ant = rotateAnt antTurnAngle ant


-- --------------------------------- Vision --------------------------------- --

updateVisionRays :: [Rectangle] -> Ant -> Ant
updateVisionRays walls ant =
    let visionRays = calcVisionRays (antPos ant) (antAngle ant) antVisionAngle antVisionResolution antVisionMaxDistance walls
    in  ant{antVisionRays = visionRays}


-- -------------------------------- Collision ------------------------------- --

canGoThere :: Vector2 -> Rectangle -> Bool
canGoThere (Vector2 x y) (Rectangle rx ry rw rh) =
    x < rx || x > rx + rw || y < ry || y > ry + rh


checkWallCollision :: [Rectangle] -> Vector2 -> Bool
checkWallCollision walls nextPos = all (canGoThere nextPos) walls


-- -------------------------------------------------------------------------- --

-- Rotate the ant by the given angle in radians wrapping around if needed
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


-- TODO Consider having this be an option. The other option is to reflect of the border.
wrapAroundAnt :: Float -> Float -> Ant -> Ant
wrapAroundAnt w h ant =
    let Vector2 x y = antPos ant
        x' = if x > right then x - w else if x < left then x + w else x
        y' = if y > top then y - h else if y < bottom then y + h else y
    in  ant{antPos = Vector2 x' y'}
    where
        top = h / 2
        bottom = -(h / 2)
        right = w / 2
        left = -(w / 2)


wrapAroundAntRaylib :: Ant -> Ant
wrapAroundAntRaylib ant =
    let w = int2Float screenWidth
        h = int2Float screenHeight
        Vector2 x y = antPos ant
        x' = if x > w then 0 else if x < 0 then w else x
        y' = if y > h then 0 else if y < 0 then h else y
    in  ant{antPos = Vector2 x' y'}


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
    in  Ant (Vector2 x y) 0 0 SeekFood rng False Center LeftSprite []


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


sortByDrawOrder :: [Entity] -> [Entity]
sortByDrawOrder = sort


visionRayToLine :: VisionRay -> (Vector2, Vector2)
visionRayToLine (VisionRay pos@(Vector2 posX posY) angle rayLength) =
    let rad = angle * deg2Rad
        x = posX + rayLength * cos rad
        y = posY + rayLength * sin rad
    in  (pos, Vector2 x y)


-- ----------------------------- PART Game Loop ----------------------------- --

initWorld :: IO World
initWorld = do
    seed <- randomIO
    let screenCenterW = int2Float screenWidth / 2
        screenCenterH = int2Float screenHeight / 2
        antEntity = PlayerAntE (mkPlayerAnt screenCenterW screenCenterH seed)
        testWall1 = Rectangle 200 200 500 300
        testWall2 = Rectangle 100 300 1000 50
        testWall3 = Rectangle 500 600 50 50
        walls = [testWall1, testWall2, testWall3] ++ borderWalls
        testPheromone = PheromoneE (Circle (Vector2 500 600) 10)
        entities = sortByDrawOrder [antEntity, testPheromone]
    window <- initWindow screenWidth screenHeight title
    setTargetFPS fps
    setTraceLogLevel LogWarning
    setMouseCursor MouseCursorCrosshair
    antTexture <- loadTexture antPng window
    return $ World window antTexture entities True walls Nothing


handleWorldInput :: World -> IO World
handleWorldInput w = do
    go <- isKeyDown KeyUp
    left <- isKeyDown KeyLeft
    right <- isKeyDown KeyRight
    visionRays <- isKeyPressed KeyV
    let toggleVisionRays = visionRays /= wRenderVisionRays w
        entities' =
            map
                ( \e -> case e of
                    PlayerAntE ant ->
                        PlayerAntE
                            ant
                                { antGo = go,
                                  antWheelPos = case (left, right) of
                                    (True, False) -> TurnLeft
                                    (False, True) -> TurnRight
                                    _ -> Center
                                }
                    AntE -> e
                    DeadAntE -> e
                    PheromoneE _ -> e
                    FoodE _ -> e
                    NestE _ -> e
                )
                (wEntities w)
    return w{wEntities = entities', wRenderVisionRays = toggleVisionRays}


updateWorld :: World -> World
updateWorld w =
    let entities = wEntities w
        walls = wWalls w
        entities' =
            map
                ( \e -> case e of
                    PlayerAntE ant ->
                        ant
                            & driveAnt walls
                            & updateVisionRays walls
                            & cycleAntSprite
                            & wrapAroundAntRaylib
                            & PlayerAntE
                    AntE -> e
                    DeadAntE -> e
                    PheromoneE _ -> e
                    FoodE _ -> e
                    NestE _ -> e
                )
                entities
    in  w{wEntities = entities'}


renderWorld :: World -> IO ()
renderWorld w = do
    f11Pressed <- isKeyPressed KeyF11
    when f11Pressed toggleFullscreen

    let wr = wWindowResources w
        antTexture = wAntTexture w
        entities = wEntities w
        renderVisionRays = wRenderVisionRays w

    forM_ entities $ \case
        PlayerAntE ant -> do
            let texW = texture'width antTexture
                texH = texture'height antTexture
                spriteRect = case antSprite ant of
                    LeftSprite -> Rectangle 0 0 (int2Float texW / 2) (int2Float texH)
                    RightSprite -> Rectangle (int2Float texW / 2) 0 (int2Float texW / 2) (int2Float texH)
                antVision = renderAntVision 200 ant
                visionRayLines = antVisionRays ant & map visionRayToLine
            when renderVisionRays $ do
                forM_ visionRayLines $ \(start, end) -> do
                    drawLineV start end green
            drawTextureCentered
                antTexture
                spriteRect
                antScale
                (antAngle ant)
                (antPos ant)
                white
            antVisionTexture <- loadTextureFromImage antVision wr
            drawTextureV antVisionTexture (Vector2 200 0) white
        AntE -> return ()
        DeadAntE -> return ()
        PheromoneE (Circle pos r) -> drawCircleV pos r blue
        FoodE (Circle pos r) -> drawCircleV pos r green
        NestE (Circle pos r) -> drawCircleV pos r red


mainGameSys :: System World
mainGameSys =
    let allSystems =
            System handleWorldInput updateWorld renderWorld
                <> drawWallsSys1
    in  allSystems
            { render = \w -> drawing $ do
                clearBackground lightGray
                drawFPS 10 10
                render allSystems w
            }


main :: IO ()
main = initWorld >>= gameLoop mainGameSys windowShouldClose
