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
main = hspec $ forM_ codecs $ \(Codec encs decs) -> do
    (refEncName, refEnc):_ <- return encs
    (refDecName, refDec):_ <- return decs

    prop (refEncName ++ "/" ++ refDecName) $ \list ->
        let v = V.fromList (map toSomeData list)
         in refDec (refEnc v) `shouldBe` Just v

    forM_ (drop 1 encs) $ \(name, enc) -> do
        prop (name ++ " vs " ++ refEncName) $ \list ->
            let v = V.fromList (map toSomeData list)
             in enc v `shouldBe` refEnc v

    forM_ (drop 1 decs) $ \(name, dec) -> do
        prop (name ++ " vs " ++ refDecName) $ \list ->
            let v = V.fromList (map toSomeData list)
                bytes = refEnc v
             in dec bytes `shouldBe` refDec bytes
