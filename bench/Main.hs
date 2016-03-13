{-# LANGUAGE GADTs #-}
import           Criterion.Main
import qualified Data.Vector    as V
import           Lib

main :: IO ()
main = do
    let sds = V.map (\i -> SomeData i (fromIntegral i) (fromIntegral i))
            $ V.enumFromTo 1 100

        benchEnc (Codec encs _) = flip map encs $ \(name, enc) ->
            bench name $ nf enc sds
        benchDec (Codec ((_, enc):_) decs) = flip map decs $ \(name, dec) ->
            let bytes = enc sds
             in bytes `seq` bench name (nf dec bytes)
        benchDec (Codec [] _) = error "benchDec with no encs"

    defaultMain
        [ bgroup "encode" $ concatMap benchEnc codecs
        , bgroup "decode" $ concatMap benchDec codecs
        ]
