{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Food where

import AntMovement (antMovementSys)
import Constants (
    antAcceleration,
    antMaxSpeed,
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
    mkAnt,
    setAt,
 )
import System.Random (mkStdGen, randomIO)
import Types (
    Ant (..),
    Container (..),
    EntityType (..),
    Food (..),
    GoDir (..),
    Nest (..),
    Sprite (..),
    WheelPos (..),
    World (..),
 )


initFoodWorld :: IO World
initFoodWorld = do
    _ <- initWindow screenWidth screenHeight "Food"
    setTargetFPS fps
    setTraceLogLevel LogWarning
    setMouseCursor MouseCursorCrosshair
    seed <- randomIO
    let antPos' = Vector2 (int2Float screenWidth / 2) (int2Float screenHeight / 2)
        playerAnt = mkAnt antPos' seed
        nest = Nest (Container 0 (calcCenteredRect antPos' collisionRectSize))
        walls = []
    return $ World playerAnt [] nest True True False True walls Nothing [] Nothing []


-- When the mouse is clicked, add a Food object at that position to foodBeingDrawn.
-- Continue incrementing foodAmount while the mouse is held down.
-- When the mouse is released, stop incrementing foodAmount and add the
--      foodBeingDrawn object to the food list.
-- Right click to remove food objects.
handleFoodInput :: World -> IO World
handleFoodInput w = do
    mousePos <- getMousePosition
    isMouseLeftPressed <- isMouseButtonDown MouseButtonLeft
    isMouseRightPressed <- isMouseButtonDown MouseButtonRight
    case (isMouseLeftPressed, isMouseRightPressed, w.wFoodBeingDrawn) of
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
            return $ w{wFoodBeingDrawn = Nothing, wFood = food : w.wFood}
        --
        -- Right click to remove food objects
        (False, True, Nothing) ->
            let foodToKeep = w.wFood & filter (not . isPointInRect mousePos . (.fContainer.cRect))
            in  return w{wFood = foodToKeep}
        --
        -- Otherwise, do nothing
        (False, False, Nothing) ->
            return w


updateFoodWorld :: World -> World
updateFoodWorld w =
    -- If the ant enters a food rectangle and antHasFood is False, set antHasFood to True
    let ant = w.wPlayerAnt
        nest = w.wNest
        foods = w.wFood

        -- Find the index of the first food object that the ant is in, if any
        antInFoodIndex = foods & findIndex (\food -> isPointInRect ant.aPos food.fContainer.cRect)

        -- If an ant has food and enters the nest rectangle, set antHasFood to False
        antInNest = isPointInRect ant.aPos nest.nContainer.cRect

        (antHasFood', antScore', nestScore', foods') =
            case (ant.aHasFood, antInNest, antInFoodIndex) of
                -- If the ant finds food, it gets 0.5 points and one food unit is removed from the food object
                (False, _, Just i) ->
                    ( True,
                      ant.aScore + 0.5,
                      nest.nContainer.cAmount,
                      let Food (Container amount rect) = (foods !! i)
                          foodObj' = Food (Container (amount - 1) rect)
                      in  setAt i foodObj' foods
                    )
                -- If the ant brings the food back to the nest, it gets 0.5 points and the nest gets a point
                (True, True, _) -> (False, ant.aScore + 0.5, nest.nContainer.cAmount + 1, foods)
                -- Otherwise, do nothing
                _ -> (ant.aHasFood, ant.aScore, nest.nContainer.cAmount, foods)

        -- Delete food when amount is 0
        foods'' = foods' & filter (\food -> food.fContainer.cAmount > 0)
        -- _ = traceShowId (antHasFood', antScore', nestScore', map foodAmount foods'')
        _ = 0 -- Deal with the formatter :(
    in  w
            { wPlayerAnt = ant{aHasFood = antHasFood', aScore = antScore'},
              wNest = nest{nContainer = nest.nContainer{cAmount = nestScore'}},
              wFood = foods''
            }


-- Use a constant rectangle size for food, and just scale the amount circle.
drawFood :: Food -> IO ()
drawFood (Food (Container amount rect)) = do
    -- Draw food as a circle
    let radius = int2Float amount * foodScale + 9
        pos = calcRectCenter rect
    drawCircleV pos radius foodColor

    -- Draw the collision rectangle
    -- drawRectangleLinesEx rect 2 black
    return () -- Deal with the formatter :(


renderFoodWorld :: World -> IO ()
renderFoodWorld w = do
    let nest = w.wNest
        nestPos = nest.nContainer.cRect & calcRectCenter
        playerAnt = w.wPlayerAnt

    -- draw the nest
    drawCircleV nestPos nestSize brown

    -- draw nest rect
    -- drawRectangleLinesEx (nestCollisionRect nest) 2 black

    -- Draw all food
    forM_ w.wFood drawFood

    -- Draw foodBeingDrawn (Note to self: I can use forM_ here, too.)
    case w.wFoodBeingDrawn of
        Just food -> drawFood food
        Nothing -> return ()

    -- If the ant has food, draw a piece of food in its mouth
    when playerAnt.aHasFood $ do
        let antPos' = playerAnt.aPos
            antAngle' = playerAnt.aAngle
            foodPiecePos = getNextPos antAngle' 20 antPos'
        drawCircleV foodPiecePos 10 foodColor


foodSys :: System World
foodSys = System handleFoodInput updateFoodWorld renderFoodWorld


foodSysWrapped :: System World
foodSysWrapped =
    let allSystems = foodSys <> antMovementSys
    in  allSystems
            { render = \w -> drawing $ do
                f11Pressed <- isKeyPressed KeyF11
                when f11Pressed toggleFullscreen
                clearBackground lightGray
                allSystems.render w
                -- drawFPS 10 10
            }


driveFood :: IO ()
driveFood =
    initFoodWorld >>= gameLoop foodSysWrapped windowShouldClose