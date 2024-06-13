module FlatlandRenderer where


data Vector3 = Vector3 {x :: Float, y :: Float, z :: Float} deriving (Show)


main :: IO ()
main = do
    let v = Vector3 1 2 3
    print v