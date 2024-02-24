import           Control.Exception (evaluate)
import           Model
import           System.Random
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Prelude.head" $ do
        it "returns the first element of a list" $ do
            head [23 ..] `shouldBe` (23 :: Int)

        it "returns the first element of an *arbitrary* list" $ do
            property $ \x xs -> head (x:xs) == (x :: Int)

        it "throws an exception if used with an empty list" $ do
            evaluate (head []) `shouldThrow` anyException

        it "makes an ant" $ do
            let gen = mkStdGen 0
            let (actualAnt, _) = mkAnt 1 2 2 gen
            let expectedAnt = Ant 1 3 3 North SeekFood 0 200
            actualAnt `shouldBe` expectedAnt
