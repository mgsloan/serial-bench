import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Lib
import Control.Applicative
import qualified Data.Vector as V

instance Arbitrary SomeData where
    arbitrary = SomeData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

main :: IO ()
main = do
    let test name func =
            prop name $ \list ->
                let v = V.fromList list
                    bs = encode v
                    mv = func bs
                 in mv `shouldBe` Just v

    hspec $ do
        test "binary" binary
        test "cereal" cereal
        test "simple" simple
