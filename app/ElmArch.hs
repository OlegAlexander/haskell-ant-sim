-- https://guide.elm-lang.org/architecture/
{-# HLINT ignore "Eta reduce" #-}
module Main where

import System.Random


-- ---------------------------------- MODEL --------------------------------- --

data Model = GameModel
    { mTick :: Int,
      mRng :: StdGen,
      mSequence :: [Int]
    }
    deriving (Show)


initModel :: IO Model
initModel = do
    seed <- randomIO
    return $ GameModel 0 (mkStdGen seed) []


-- --------------------------------- UPDATE --------------------------------- --

data Msg = Next | Wait


update :: Msg -> Model -> Model
update msg model = case msg of
    Next ->
        let (n, rng') = random (mRng model)
            tick = mTick model
        in  GameModel (tick + 1) rng' (n : mSequence model)
    Wait -> model{mTick = mTick model + 1}


-- ---------------------------------- VIEW ---------------------------------- --

view :: Model -> IO ()
view model = print (mTick model, mSequence model)


-- ---------------------------------- LOOP ---------------------------------- --

parseMsg :: String -> Msg
parseMsg "next" = Next
parseMsg "wait" = Wait
parseMsg _ = error "Unknown command"


loop
    :: (m -> IO ()) -- View function
    -> (msg -> m -> m) -- Update function
    -> (String -> msg) -- Message parsing function
    -> m -- Initial model
    -> IO ()
loop viewFunc updateFunc parseMsgFunc model = do
    viewFunc model
    putStrLn "Enter command: "
    command <- getLine
    let msg = parseMsgFunc command
    let newModel = updateFunc msg model
    loop viewFunc updateFunc parseMsgFunc newModel


-- ---------------------------------- MAIN ---------------------------------- --

main :: IO ()
main = do
    model <- initModel
    loop view update parseMsg model
