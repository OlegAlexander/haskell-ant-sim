-- https://guide.elm-lang.org/architecture/
{-# HLINT ignore "Eta reduce" #-}
module Main where

import System.Random


-- ---------------------------------- MODEL --------------------------------- --

data GameModel = GameModel
    { mTick :: Int,
      mRng :: StdGen,
      mSequence :: [Int]
    }
    deriving (Show)


initModel :: IO GameModel
initModel = do
    seed <- randomIO
    return $ GameModel 0 (mkStdGen seed) []


-- --------------------------------- UPDATE --------------------------------- --

data Msg = Next | Wait


update :: Msg -> GameModel -> GameModel
update msg model = case msg of
    Next ->
        let (n, rng') = random (mRng model)
            tick = mTick model
        in  GameModel (tick + 1) rng' (n : mSequence model)
    Wait -> model{mTick = mTick model + 1}


-- ---------------------------------- VIEW ---------------------------------- --

view :: GameModel -> IO ()
view model = print (mTick model, mSequence model)


-- ---------------------------------- LOOP ---------------------------------- --

parseMsg :: String -> Msg
parseMsg "next" = Next
parseMsg "wait" = Wait
parseMsg _ = error "Unknown command"


loop :: (GameModel -> IO ()) -> (Msg -> GameModel -> GameModel) -> GameModel -> IO ()
loop viewFunc updateFunc model = do
    viewFunc model
    putStrLn "Enter 'next' or 'wait': "
    command <- getLine
    let msg = parseMsg command
    let newModel = updateFunc msg model
    loop viewFunc updateFunc newModel


main :: IO ()
main = do
    model <- initModel
    loop view update model
