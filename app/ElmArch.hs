-- https://guide.elm-lang.org/architecture/
{-# HLINT ignore "Eta reduce" #-}
module Main where

import System.Random


-- ---------------------------------- MODEL --------------------------------- --

data Model = Model
    { mTick :: Int,
      mRng :: StdGen,
      mSequence :: [Int]
    }
    deriving (Show)


initModel :: Int -> Model
initModel seed = Model 0 (mkStdGen seed) []


-- --------------------------------- UPDATE --------------------------------- --

data Msg = Next | Wait


update :: Msg -> Model -> Model
update msg model = case msg of
    Next ->
        let (n, rng') = random (mRng model)
            tick = mTick model
        in  Model (tick + 1) rng' (n : mSequence model)
    Wait -> model{mTick = mTick model + 1}


-- ---------------------------------- VIEW ---------------------------------- --

view :: Model -> IO ()
view model = print (mTick model, mSequence model)


-- ---------------------------------- LOOP ---------------------------------- --

loop :: Model -> IO ()
loop model = do
    view model
    putStrLn "Enter 'next' or 'wait': "
    command <- getLine
    let msg = case command of
            "next" -> Next
            "wait" -> Wait
            _ -> error "Unknown command"
    let newModel = update msg model
    loop newModel


main :: IO ()
main = do
    seed <- randomIO
    loop $ initModel seed
