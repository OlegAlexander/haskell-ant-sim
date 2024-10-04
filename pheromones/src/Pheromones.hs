{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Pheromones where

import AntMovement (antMovementSys)
import Constants (
    antAcceleration,
    antMaxSpeed,
    antPng,
    antTurnAngle,
    collisionRectSize,
    foodColor,
    foodGrowthAmount,
    foodScale,
    fps,
    nestSize,
    screenHeight,
    screenWidth,
 )
import Control.Monad (forM_, when)
import Data.Fixed (mod')
import Data.Foldable (find)
import Data.Function ((&))
import Data.List (findIndex)
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShowId)
import GHC.Float (int2Float)
import Raylib.Core (
    clearBackground,
    getMousePosition,
    initWindow,
    isKeyDown,
    isKeyPressed,
    isMouseButtonDown,
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
    MouseButton (MouseButtonLeft),
    MouseCursor (MouseCursorCrosshair),
    Rectangle (Rectangle),
    TraceLogLevel (LogWarning),
 )
import Raylib.Types.Core (MouseButton (MouseButtonRight), Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black, blue, brown, green, lightGray, red)
import Shared (
    System (..),
    calcCenteredRect,
    calcRectCenter,
    gameLoop,
    getNextPos,
    isPointInRect,
    setAt,
 )
import System.Random (mkStdGen)
import Types (
    Ant (..),
    Container (..),
    EntityType (..),
    Food (..),
    GoDir (..),
    Mode (..),
    Nest (..),
    Sprite (..),
    WheelPos (..),
    World (..),
 )


initPheromoneWorld :: IO World
initPheromoneWorld = do
    let screenCenterW = int2Float screenWidth / 2
        screenCenterH = int2Float screenHeight / 2
        walls = []
    window <- initWindow screenWidth screenHeight "Food"
    setTargetFPS fps
    setTraceLogLevel LogWarning
    setMouseCursor MouseCursorCrosshair
    antTexture <- loadTexture antPng window
    let rng = mkStdGen 0
        antPos = Vector2 screenCenterW screenCenterH
        nest = Nest (Container 0 (calcCenteredRect antPos collisionRectSize))
        playerAnt = Ant antPos 0 0 SeekFood rng Stop Center LeftSprite [] 0 0 False 0
    return $ World window antTexture playerAnt nest True True False True walls Nothing [] Nothing


-- When the mouse is clicked, add a Food object at that position to foodBeingDrawn.
-- Continue incrementing foodAmount while the mouse is held down.
-- When the mouse is released, stop incrementing foodAmount and add the
--      foodBeingDrawn object to the food list.
-- Right click to remove food objects.
handlePheromoneInput :: World -> IO World
handlePheromoneInput w = do
    mousePos <- getMousePosition
    isMouseLeftPressed <- isMouseButtonDown MouseButtonLeft
    isMouseRightPressed <- isMouseButtonDown MouseButtonRight
    case (isMouseLeftPressed, isMouseRightPressed, wFoodBeingDrawn w) of
        --
        -- Left click to add food objects
        (True, _, Nothing) ->
            let collisionRect = calcCenteredRect mousePos collisionRectSize
                foodBeingDrawn = Food (Container foodGrowthAmount collisionRect)
            in  return $ w{wFoodBeingDrawn = Just foodBeingDrawn}
        --
        -- Increment foodAmount while the mouse is held down
        (True, _, Just (Food (Container amount rect))) ->
            let foodBeingDrawn = Food (Container (amount + foodGrowthAmount) rect)
            in  return $ w{wFoodBeingDrawn = Just foodBeingDrawn}
        (False, _, Just food) ->
            return $ w{wFoodBeingDrawn = Nothing, wFood = food : wFood w}
        --
        -- Right click to remove food objects
        (False, True, Nothing) ->
            let foodToKeep = filter (not . isPointInRect mousePos . (containerRect . foodContainer)) (wFood w)
            in  return w{wFood = foodToKeep}
        --
        -- Otherwise, do nothing
        (False, False, Nothing) ->
            return w


updatePheromoneWorld :: World -> World
updatePheromoneWorld w =
    -- If the ant enters a food rectangle and antHasFood is False, set antHasFood to True
    let ant = wPlayerAnt w
        nest = wNest w
        foods = wFood w

        -- Find the index of the first food object that the ant is in, if any
        antInFoodIndex = findIndex (\food -> isPointInRect (antPos ant) (food & foodContainer & containerRect)) foods

        -- If an ant has food and enters the nest rectangle, set antHasFood to False
        antInNest = isPointInRect (antPos ant) (nest & nestContainer & containerRect)

        (antHasFood', antScore', nestScore', foods') =
            case (antHasFood ant, antInNest, antInFoodIndex) of
                -- If the ant finds food, it gets 0.5 points and one food unit is removed from the food object
                (False, _, Just i) ->
                    ( True,
                      antScore ant + 0.5,
                      nest & nestContainer & containerAmount,
                      let Food (Container amount rect) = (foods !! i)
                          foodObj' = Food (Container (amount - 1) rect)
                      in  setAt i foodObj' foods
                    )
                -- If the ant brings the food back to the nest, it gets 0.5 points and the nest gets a point
                (True, True, _) -> (False, antScore ant + 0.5, (nest & nestContainer & containerAmount) + 1, foods)
                -- Otherwise, do nothing
                _ -> (antHasFood ant, antScore ant, nest & nestContainer & containerAmount, foods)

        -- Delete food when amount is 0
        foods'' = filter (\food -> (food & foodContainer & containerAmount) > 0) foods'
        -- _ = traceShowId (antHasFood', antScore', nestScore', map foodAmount foods'')
        _ = 0 -- Deal with the formatter :(
    in  w
            { wPlayerAnt = ant{antHasFood = antHasFood', antScore = antScore'},
              wNest = nest{nestContainer = (nest & nestContainer){containerAmount = nestScore'}},
              wFood = foods''
            }


-- Use a constant rectangle size for food, and just scale the amount circle.
drawPheromone :: Food -> IO ()
drawPheromone (Food (Container amount rect)) = do
    -- Draw food as a circle
    let radius = int2Float amount * foodScale + 9
        pos = calcRectCenter rect
    drawCircleV pos radius foodColor

    -- Draw the collision rectangle
    -- drawRectangleLinesEx rect 2 black
    return () -- Deal with the formatter :(


renderPheromoneWorld :: World -> IO ()
renderPheromoneWorld w = do
    let nest = wNest w
        nestPos = calcRectCenter (nest & nestContainer & containerRect)
        playerAnt = wPlayerAnt w

    -- draw the nest
    drawCircleV nestPos nestSize brown

    -- draw nest rect
    -- drawRectangleLinesEx (nestCollisionRect nest) 2 black

    -- Draw all food
    forM_ (wFood w) drawPheromone

    -- Draw foodBeingDrawn (Note to self: I can use forM_ here, too.)
    case wFoodBeingDrawn w of
        Just food -> drawPheromone food
        Nothing -> return ()

    -- If the ant has food, draw a piece of food in its mouth
    when (antHasFood playerAnt) $ do
        let antPos' = antPos playerAnt
            antAngle' = antAngle playerAnt
            foodPiecePos = getNextPos antAngle' 20 antPos'
        drawCircleV foodPiecePos 10 foodColor


pheromoneSys :: System World
pheromoneSys = System handlePheromoneInput updatePheromoneWorld renderPheromoneWorld


pheromoneSysWrapped :: System World
pheromoneSysWrapped =
    let allSystems = pheromoneSys <> antMovementSys
    in  allSystems
            { render = \w -> drawing $ do
                f11Pressed <- isKeyPressed KeyF11
                when f11Pressed toggleFullscreen
                clearBackground lightGray
                render allSystems w
                -- drawFPS 10 10
            }


drivePheromones :: IO ()
drivePheromones =
    initPheromoneWorld >>= gameLoop pheromoneSysWrapped windowShouldClose