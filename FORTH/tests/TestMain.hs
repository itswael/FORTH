-- file: test/TestMain.hs
import Test.Hspec
import Test.QuickCheck
import MyModule (myFunction)  -- Import your own modules

main :: IO ()
main = hspec $ do
  describe "myFunction" $ do
    it "returns the correct result for input 1" $ do
      myFunction 1 `shouldBe` 2  -- Example test