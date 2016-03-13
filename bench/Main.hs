{-# LANGUAGE GADTs #-}
import           Criterion.Main
import qualified Data.Vector    as V
import           Lib

main :: IO ()
main = do
    let sds = V.map (\i -> SomeData i (fromIntegral i) (fromIntegral i))
            $ V.enumFromTo 1 100

        benchEnc (Codec name enc _) =
            bench name $ nf enc sds
        benchDec (Codec name enc dec) =
            let bytes = enc sds
             in bytes `seq` bench name (nf dec bytes)

    defaultMain
        [ bgroup "encode" $ map benchEnc codecs
        , bgroup "decode" $ map benchDec codecs
        ]
