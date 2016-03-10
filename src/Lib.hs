{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Lib
    ( SomeData (..)
    , binary
    , cereal
    , simple
    , encode
    , encodeLE
    , simpleLE
    , simpleEncode
    ) where

import Data.Int
import Data.Word
import qualified Data.Binary as B
import Data.Binary.Get (getWord64be)
import qualified Data.Serialize as C
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Monoid ((<>))
import Data.Vector.Binary ()
import Data.Vector.Serialize ()
import Control.Monad.ST
import Control.DeepSeq
import qualified Data.ByteString.Unsafe as SU
import Data.Bits ((.|.), shiftL)
import Data.ByteString.Internal (ByteString (PS), accursedUnutterablePerformIO, unsafeCreate)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.Ptr (Ptr)
import qualified Data.Vector

data SomeData = SomeData !Int64 !Int64 !Int64
    deriving (Eq, Show)
instance NFData SomeData where
    rnf x = x `seq` ()

instance B.Binary SomeData where
    get = SomeData <$> B.get <*> B.get <*> B.get
    put (SomeData x y z) = do
        B.put x
        B.put y
        B.put z
    {-# INLINE get #-}
    {-# INLINE put #-}

instance C.Serialize SomeData where
    get = SomeData <$> C.get <*> C.get <*> C.get
    put (SomeData x y z) = do
        C.put x
        C.put y
        C.put z
    {-# INLINE get #-}
    {-# INLINE put #-}

encode :: V.Vector v SomeData => v SomeData -> ByteString
encode v = L.toStrict
         $ Builder.toLazyByteString
         $ Builder.int64BE (fromIntegral $ V.length v)
        <> V.foldr (\sd b -> go sd <> b) mempty v
  where
    go (SomeData x y z)
        = Builder.int64BE x
       <> Builder.int64BE y
       <> Builder.int64BE z

encodeLE :: V.Vector v SomeData => v SomeData -> ByteString
encodeLE v = L.toStrict
         $ Builder.toLazyByteString
         $ Builder.int64LE (fromIntegral $ V.length v)
        <> V.foldr (\sd b -> go sd <> b) mempty v
  where
    go (SomeData x y z)
        = Builder.int64LE x
       <> Builder.int64LE y
       <> Builder.int64LE z

binary
    :: B.Binary (v SomeData)
    => ByteString
    -> Maybe (v SomeData)
binary = either
            (const Nothing)
            (\(lbs, _, x) ->
                if L.null lbs
                    then Just x
                    else Nothing)
       . B.decodeOrFail
       . L.fromStrict

cereal
    :: C.Serialize (v SomeData)
    => ByteString
    -> Maybe (v SomeData)
cereal = either (const Nothing) Just . C.decode

simple
    :: V.Vector v SomeData
    => ByteString
    -> Maybe (v SomeData)
simple bs0 = runST $
    readInt64 bs0 $ \bs1 len -> do
        mv <- MV.new len
        let loop idx bs
                | idx >= len = Just <$> V.unsafeFreeze mv
                | otherwise =
                    readInt64 bs  $ \bsX x ->
                    readInt64 bsX $ \bsY y ->
                    readInt64 bsY $ \bsZ z -> do
                        MV.unsafeWrite mv idx (SomeData x y z)
                        loop (idx + 1) bsZ
        loop 0 bs1
  where
    readInt64 bs f
        | S.length bs < 8 = return Nothing
        | otherwise = f
            (SU.unsafeDrop 8 bs)
            (fromIntegral $ word64be bs)

word64be :: ByteString -> Word64
word64be = \s ->
              (fromIntegral (s `SU.unsafeIndex` 0) `shiftL` 56) .|.
              (fromIntegral (s `SU.unsafeIndex` 1) `shiftL` 48) .|.
              (fromIntegral (s `SU.unsafeIndex` 2) `shiftL` 40) .|.
              (fromIntegral (s `SU.unsafeIndex` 3) `shiftL` 32) .|.
              (fromIntegral (s `SU.unsafeIndex` 4) `shiftL` 24) .|.
              (fromIntegral (s `SU.unsafeIndex` 5) `shiftL` 16) .|.
              (fromIntegral (s `SU.unsafeIndex` 6) `shiftL`  8) .|.
              (fromIntegral (s `SU.unsafeIndex` 7) )
{-# INLINE word64be #-}

simpleLE
    :: V.Vector v SomeData
    => ByteString
    -> Maybe (v SomeData)
simpleLE bs0 = runST $
    readInt64 bs0 $ \bs1 len -> do
        mv <- MV.new len
        let loop idx bs
                | idx >= len = Just <$> V.unsafeFreeze mv
                | otherwise =
                    readInt64 bs  $ \bsX x ->
                    readInt64 bsX $ \bsY y ->
                    readInt64 bsY $ \bsZ z -> do
                        MV.unsafeWrite mv idx (SomeData x y z)
                        loop (idx + 1) bsZ
        loop 0 bs1
  where
    readInt64 bs f
        | S.length bs < 8 = return Nothing
        | otherwise = f
            (SU.unsafeDrop 8 bs)
            (fromIntegral $ word64le bs)

word64le :: ByteString -> Word64
#if 0
word64le = \s ->
              (fromIntegral (s `SU.unsafeIndex` 7) `shiftL` 56) .|.
              (fromIntegral (s `SU.unsafeIndex` 6) `shiftL` 48) .|.
              (fromIntegral (s `SU.unsafeIndex` 5) `shiftL` 40) .|.
              (fromIntegral (s `SU.unsafeIndex` 4) `shiftL` 32) .|.
              (fromIntegral (s `SU.unsafeIndex` 3) `shiftL` 24) .|.
              (fromIntegral (s `SU.unsafeIndex` 2) `shiftL` 16) .|.
              (fromIntegral (s `SU.unsafeIndex` 1) `shiftL`  8) .|.
              (fromIntegral (s `SU.unsafeIndex` 0) )
#endif
word64le (PS x s _) =
    accursedUnutterablePerformIO $ withForeignPtr x $ \p -> peekByteOff p s
{-# INLINE word64le #-}

class Simple a where
    simpleSize :: Either Int (a -> Int)
    simplePoke :: Ptr byte -> Int -> a -> IO ()
    simplePeek :: Ptr byte -> Int -> IO a
instance Simple Int64 where
    simpleSize = Left 8
    simplePoke = pokeByteOff
    simplePeek = peekByteOff
instance Simple SomeData where
    simpleSize = Left 24
    simplePoke p s (SomeData x y z) = do
        simplePoke p s x
        simplePoke p (s + 8) y
        simplePoke p (s + 16) z
    simplePeek p s = SomeData
        <$> simplePeek p s
        <*> simplePeek p (s + 8)
        <*> simplePeek p (s + 16)
instance Simple a => Simple (Data.Vector.Vector a) where
    simpleSize = Right $ \v ->
        case simpleSize of
            Left s -> s * V.length v + 8
            Right f -> V.sum (V.map f v) + 8
    simplePoke p s v = do
        simplePoke p s (fromIntegral (V.length v) :: Int64)
        let getSize =
                case simpleSize of
                    Left x -> const x
                    Right f -> f
            loop i s'
                | i >= V.length v = return ()
                | otherwise = do
                    let x = V.unsafeIndex v i
                    simplePoke p s' x
                    loop (i + 1) (s' + getSize x)
        loop 0 (s + 8)
    simplePeek p s = do
        len <- simplePeek p s
        mv <- MV.new (fromIntegral (len :: Int64))
        let getSize =
                case simpleSize of
                    Left x -> const x
                    Right f -> f
            loop i s'
                | i >= fromIntegral len = V.unsafeFreeze mv
                | otherwise = do
                    x <- simplePeek p s'
                    MV.unsafeWrite mv i x
                    loop (i + 1) (s' + getSize x)
        loop 0 s

simpleEncode :: Simple a => a -> ByteString
simpleEncode x = unsafeCreate (either id ($ x) simpleSize) (\p -> simplePoke p 0 x)
