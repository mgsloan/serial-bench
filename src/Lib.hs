{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
module Lib
    ( SomeData (..)
    , Codec (..)
    , codecs

    , encodeBinary
    , decodeBinary

    , encodeCereal
    , decodeCereal

    , encodeSimpleBE
    , decodeSimpleBE

    , encodeSimpleLE
    , decodeSimpleLE

    , encodeSimpleClass
    , decodeSimpleClass
    , decodeSimpleClassEx
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
import Foreign.Storable (peekByteOff, pokeByteOff, Storable, sizeOf)
import Foreign.Ptr (Ptr)
import qualified Data.Vector
import Control.Monad.Primitive (PrimMonad (..), unsafePrimToIO)
import GHC.Base   ( unsafeCoerce# )
import Control.Exception (Exception, catch, throwIO)
import Data.Typeable (Typeable)
import qualified Data.Vector.Unboxed.Mutable
import qualified Control.Monad.Fail as Fail
import Unsafe.Coerce (unsafeCoerce)

data Codec where
    Codec :: NFData binary
          => String
          -> (Data.Vector.Vector SomeData -> binary)
          -> (binary -> Maybe (Data.Vector.Vector SomeData))
          -> Codec

codecs :: [Codec]
codecs =
    [ Codec "binary" encodeBinary decodeBinary
    , Codec "cereal" encodeCereal decodeCereal
    , Codec "simpleBE" encodeSimpleBE decodeSimpleBE
    , Codec "simpleLE" encodeSimpleLE decodeSimpleLE
    , Codec "simpleClass" encodeSimpleClass decodeSimpleClass
    , Codec "simpleClassEx" encodeSimpleClass decodeSimpleClassEx
    ]

data SomeData = SomeData !Int64 !Word8 !Double
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

encodeSimpleBE :: V.Vector v SomeData => v SomeData -> ByteString
encodeSimpleBE v = L.toStrict
         $ Builder.toLazyByteString
         $ Builder.int64BE (fromIntegral $ V.length v)
        <> V.foldr (\sd b -> go sd <> b) mempty v
  where
    go (SomeData x y z)
        = Builder.int64BE x
       <> Builder.word8 y
       <> Builder.doubleBE z

encodeSimpleLE :: V.Vector v SomeData => v SomeData -> ByteString
encodeSimpleLE v = L.toStrict
         $ Builder.toLazyByteString
         $ Builder.int64LE (fromIntegral $ V.length v)
        <> V.foldr (\sd b -> go sd <> b) mempty v
  where
    go (SomeData x y z)
        = Builder.int64LE x
       <> Builder.word8 y
       <> Builder.doubleLE z

encodeBinary
    :: B.Binary (v SomeData)
    => v SomeData
    -> L.ByteString
encodeBinary = B.encode

decodeBinary
    :: B.Binary (v SomeData)
    => L.ByteString
    -> Maybe (v SomeData)
decodeBinary = either
            (const Nothing)
            (\(lbs, _, x) ->
                if L.null lbs
                    then Just x
                    else Nothing)
       . B.decodeOrFail

encodeCereal
    :: C.Serialize (v SomeData)
    => v SomeData
    -> ByteString
encodeCereal = C.encode

decodeCereal
    :: C.Serialize (v SomeData)
    => ByteString
    -> Maybe (v SomeData)
decodeCereal = either (const Nothing) Just . C.decode

decodeSimpleBE
    :: V.Vector v SomeData
    => ByteString
    -> Maybe (v SomeData)
decodeSimpleBE bs0 = runST $
    readInt64 bs0 $ \bs1 len -> do
        let len' = fromIntegral len
        mv <- MV.new len'
        let loop idx bs
                | idx >= len' = Just <$> V.unsafeFreeze mv
                | otherwise =
                    readInt64  bs  $ \bsX x ->
                    readWord8  bsX $ \bsY y ->
                    readDouble bsY $ \bsZ z -> do
                        MV.unsafeWrite mv idx (SomeData x y z)
                        loop (idx + 1) bsZ
        loop 0 bs1
  where
    readInt64 bs f
        | S.length bs < 8 = return Nothing
        | otherwise = f
            (SU.unsafeDrop 8 bs)
            (fromIntegral $ word64be bs :: Int64)

    readWord8 bs f
        | S.length bs < 1 = return Nothing
        | otherwise = f
            (SU.unsafeDrop 1 bs)
            (bs `SU.unsafeIndex` 0)

    -- probably not safe enough
    readDouble bs f
        | S.length bs < 8 = return Nothing
        | otherwise = f
            (SU.unsafeDrop 8 bs)
            (unsafeCoerce $ word64be bs :: Double)

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

decodeSimpleLE
    :: V.Vector v SomeData
    => ByteString
    -> Maybe (v SomeData)
decodeSimpleLE bs0 = runST $
    readInt64 bs0 $ \bs1 len -> do
        let len' = fromIntegral len
        mv <- MV.new len'
        let loop idx bs
                | idx >= len' = Just <$> V.unsafeFreeze mv
                | otherwise =
                    readInt64  bs  $ \bsX x ->
                    readWord8  bsX $ \bsY y ->
                    readDouble bsY $ \bsZ z -> do
                        MV.unsafeWrite mv idx (SomeData x y z)
                        loop (idx + 1) bsZ
        loop 0 bs1
  where
    readInt64 bs f
        | S.length bs < 8 = return Nothing
        | otherwise = f
            (SU.unsafeDrop 8 bs)
            (fromIntegral $ word64le bs :: Int64)
    {-# INLINE readInt64 #-}

    readWord8 bs f
        | S.length bs < 1 = return Nothing
        | otherwise = f
            (SU.unsafeDrop 1 bs)
            (bs `SU.unsafeIndex` 0)

    readDouble bs f
        | S.length bs < 8 = return Nothing
        | otherwise = f
            (SU.unsafeDrop 8 bs)
            (doublele bs)
{-# INLINE decodeSimpleLE #-}

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

doublele :: ByteString -> Double
doublele (PS x s _) =
    accursedUnutterablePerformIO $ withForeignPtr x $ \p -> peekByteOff p s

type Total = Int
type Offset = Int
newtype Peek s a = Peek
    { runPeek :: forall r byte.
        Total
     -> Ptr byte
     -> Offset
     -> (Offset -> a -> IO (Maybe r))
     -> IO (Maybe r)
    }
    deriving Functor
instance Applicative (Peek s) where
    pure x = Peek (\_ _ offset k -> k offset x)
    Peek f <*> Peek g = Peek $ \total ptr offset1 k ->
        f total ptr offset1 $ \offset2 f' ->
        g total ptr offset2 $ \offset3 g' ->
        k offset3 (f' g')
    Peek f *> Peek g = Peek $ \total ptr offset1 k ->
        f total ptr offset1 $ \offset2 _ ->
        g total ptr offset2 k
instance Monad (Peek s) where
    return = pure
    (>>) = (*>)
    Peek x >>= f = Peek $ \total ptr offset1 k ->
        x total ptr offset1 $ \offset2 x' ->
        runPeek (f x') total ptr offset2 k
    fail = Fail.fail
instance Fail.MonadFail (Peek s) where
    fail _ = Peek $ \_ _ _ _ -> pure Nothing
instance PrimMonad (Peek s) where
    type PrimState (Peek s) = s
    primitive action = Peek $ \_ _ offset k -> do
        x <- primitive (unsafeCoerce# action)
        k offset x

decodeSimpleClass :: Simple a
            => ByteString
            -> Maybe a
decodeSimpleClass (PS x s len) =
    accursedUnutterablePerformIO $ withForeignPtr x $ \p ->
        let total = len + s
            final offset y
                | offset == total = return (Just y)
                | otherwise = return Nothing
         in runPeek simplePeek (len + s) p s final

newtype OffsetRef = OffsetRef
    (Data.Vector.Unboxed.Mutable.MVector RealWorld Offset)

newOffsetRef :: Int -> IO OffsetRef
newOffsetRef x = OffsetRef <$> MV.replicate 1 x

readOffsetRef :: OffsetRef -> IO Int
readOffsetRef (OffsetRef mv) = MV.unsafeRead mv 0

writeOffsetRef :: OffsetRef -> Int -> IO ()
writeOffsetRef (OffsetRef mv) x = MV.unsafeWrite mv 0 x

newtype PeekEx s a = PeekEx
    { runPeekEx :: forall byte.
        Total
     -> Ptr byte
     -> OffsetRef
     -> IO a
    }
    deriving Functor
instance Applicative (PeekEx s) where
    pure x = PeekEx (\_ _ _ -> pure x)
    PeekEx f <*> PeekEx g = PeekEx $ \total ptr ref ->
        f total ptr ref <*> g total ptr ref
    PeekEx f *> PeekEx g = PeekEx $ \total ptr ref ->
        f total ptr ref *>
        g total ptr ref
instance Monad (PeekEx s) where
    return = pure
    (>>) = (*>)
    PeekEx x >>= f = PeekEx $ \total ptr ref -> do
        x' <- x total ptr ref
        runPeekEx (f x') total ptr ref
    fail = Fail.fail
instance Fail.MonadFail (PeekEx s) where
    fail _ = PeekEx $ \_ _ _ -> throwIO NotEnoughBytes
instance PrimMonad (PeekEx s) where
    type PrimState (PeekEx s) = s
    primitive action = PeekEx $ \_ _ _ ->
        primitive (unsafeCoerce# action)

decodeSimpleClassEx :: Simple a
              => ByteString
              -> Maybe a
decodeSimpleClassEx (PS x s len) =
    accursedUnutterablePerformIO $ withForeignPtr x $ \p -> do
        let total = len + s
        offsetRef <- newOffsetRef s
        let runner = do
                y <- runPeekEx simplePeekEx (len + s) p offsetRef
                offset <- readOffsetRef offsetRef
                return $ if offset == total
                    then Just y
                    else Nothing
        runner `catch` \NotEnoughBytes -> return Nothing

data NotEnoughBytes = NotEnoughBytes
    deriving (Show, Typeable)
instance Exception NotEnoughBytes

storablePeek :: forall s a. Storable a => Peek s a
storablePeek = Peek $ \total ptr offset k ->
    let offset' = offset + needed
        needed = sizeOf (undefined :: a)
     in if total >= offset'
            then do
                x <- peekByteOff ptr offset
                k offset' x
            else return Nothing

storablePeekEx :: forall s a. Storable a => PeekEx s a
storablePeekEx = PeekEx $ \total ptr offsetRef -> do
    offset <- readOffsetRef offsetRef
    let offset' = offset + needed
        needed = sizeOf (undefined :: a)
    if total >= offset'
        then do
            writeOffsetRef offsetRef offset'
            peekByteOff ptr offset
        else throwIO NotEnoughBytes

class Simple a where
    simpleSize :: Either Int (a -> Int)
    simplePoke :: Ptr byte -> Int -> a -> IO ()
    simplePeek :: Peek s a
    simplePeekEx :: PeekEx s a
instance Simple Int64 where
    simpleSize = Left 8
    simplePoke = pokeByteOff
    simplePeek = storablePeek
    simplePeekEx = storablePeekEx
instance Simple Word8 where
    simpleSize = Left 1
    simplePoke = pokeByteOff
    simplePeek = storablePeek
    simplePeekEx = storablePeekEx
instance Simple Double where
    simpleSize = Left 8
    simplePoke = pokeByteOff
    simplePeek = storablePeek
    simplePeekEx = storablePeekEx
instance Simple SomeData where
    simpleSize = Left 17
    simplePoke p s (SomeData x y z) = do
        simplePoke p s x
        simplePoke p (s + 8) y
        simplePoke p (s + 9) z
    simplePeek = SomeData
        <$> simplePeek
        <*> simplePeek
        <*> simplePeek
    simplePeekEx = SomeData
        <$> simplePeekEx
        <*> simplePeekEx
        <*> simplePeekEx
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
    simplePeek = do
        len :: Int64 <- simplePeek
        let len' = fromIntegral len
        mv <- MV.new len'
        let loop i
                | i >= len' = V.unsafeFreeze mv
                | otherwise = do
                    x <- simplePeek
                    MV.unsafeWrite mv i x
                    loop $! i + 1
        loop 0
    simplePeekEx = do
        len :: Int64 <- simplePeekEx
        let len' = fromIntegral len
        mv <- MV.new len'
        let loop i
                | i >= len' = V.unsafeFreeze mv
                | otherwise = do
                    x <- simplePeekEx
                    MV.unsafeWrite mv i x
                    loop $! i + 1
        loop 0

encodeSimpleClass :: Simple a => a -> ByteString
encodeSimpleClass x = unsafeCreate (either id ($ x) simpleSize) (\p -> simplePoke p 0 x)
