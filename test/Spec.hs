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
    let test' enc name func =
            prop name $ \list ->
                let v = V.fromList list
                    bs = enc v
                    mv = func bs
                 in mv `shouldBe` Just v
        test = test' encode

    hspec $ do
        describe "decoding" $ do
            test "binary" binary
            test "cereal" cereal
            test "simple" simple
            test' encodeLE "simpleLE" simpleLE
            test' encodeLE "simpleClass" simpleClass
            test' encodeLE "simpleClassEx" simpleClassEx

        describe "encode" $ do
            prop "simpleEncode" $ \list ->
                let v = V.fromList list
                 in simpleEncode v `shouldBe` encodeLE v
