import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Lib
import Control.Applicative
import qualified Data.Vector as V
import Control.Monad (forM_)

instance Arbitrary SomeData where
    arbitrary = SomeData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

main :: IO ()
main = hspec $ forM_ codecs $ \(Codec name enc dec) ->
            prop name $ \list ->
                let v = V.fromList (list :: [SomeData])
                    bs = enc v
                    mv = dec bs
                 in mv `shouldBe` Just v
