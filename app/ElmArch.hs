-- https://guide.elm-lang.org/architecture/
{-# HLINT ignore "Eta reduce" #-}
module Main where

import Data.Vector.Generic (new)
import System.Random


-- ---------------------------------- MODEL --------------------------------- --

data Model = Model {mRng :: StdGen, mSequence :: [Int]} deriving (Show)


initModel :: Int -> Model
initModel seed = Model (mkStdGen seed) []


-- --------------------------------- UPDATE --------------------------------- --

data Msg = Next


update :: Msg -> Model -> Model
update msg model = case msg of
    Next ->
        let (n, rng') = random (mRng model)
        in  Model rng' (n : mSequence model)


-- ---------------------------------- VIEW ---------------------------------- --

view :: Model -> IO ()
view model = print $ mSequence model


-- ---------------------------------- LOOP ---------------------------------- --

loop :: Model -> IO ()
loop model = do
    view model
    putStrLn "Enter 'next':"
    command <- getLine
    let msg = case command of
            "next" -> Next
            _ -> error "Unknown command"
    let newModel = update msg model
    loop newModel


main :: IO ()
main = do
    seed <- randomIO
    loop $ initModel seed
