{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
module Lib
    ( SomeData (..)
    , binary
    , cereal
    , simple
    , encode
    , encodeLE
    , simpleLE
    , simpleEncode
    , simpleClass
    , simpleClassEx
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
    {-# INLINE readInt64 #-}
{-# INLINE simpleLE #-}

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
instance PrimMonad (Peek s) where
    type PrimState (Peek s) = s
    primitive action = Peek $ \_ _ offset k -> do
        x <- primitive (unsafeCoerce# action)
        k offset x

simpleClass :: Simple a
            => ByteString
            -> Maybe a
simpleClass (PS x s len) =
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
instance PrimMonad (PeekEx s) where
    type PrimState (PeekEx s) = s
    primitive action = PeekEx $ \_ _ _ ->
        primitive (unsafeCoerce# action)

simpleClassEx :: Simple a
              => ByteString
              -> Maybe a
simpleClassEx (PS x s len) =
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
instance Simple SomeData where
    simpleSize = Left 24
    simplePoke p s (SomeData x y z) = do
        simplePoke p s x
        simplePoke p (s + 8) y
        simplePoke p (s + 16) z
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

simpleEncode :: Simple a => a -> ByteString
simpleEncode x = unsafeCreate (either id ($ x) simpleSize) (\p -> simplePoke p 0 x)
