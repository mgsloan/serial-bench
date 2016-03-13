import           Control.Monad             (forM_)
import qualified Data.Vector               as V
import           Lib
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Arbitrary

newtype ArbSomeData = ArbSomeData { toSomeData :: SomeData }
    deriving (Show, Eq)

instance Arbitrary ArbSomeData where
    arbitrary = fmap ArbSomeData $ SomeData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

main :: IO ()
main = hspec $ forM_ codecs $ \(Codec name enc dec) ->
            prop name $ \list ->
                let v = V.fromList (map toSomeData list)
                    bs = enc v
                    mv = dec bs
                 in mv `shouldBe` Just v
