{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- | There are typically two different categories of data structures in Haskell: immutable and mutable.
-- The former are preferable for reasoning about code and simplifying concurrent code. The latter,
-- however, provide much better performance for some algorithms.
--
-- The goal of this module is to provide a compromise for a common use case: appending to the end of a sequence
-- of values. Under the surface, this module uses mutable vectors and therefore provides cheap appending.
-- However, it wraps these mutable buffers with an immutable layer that will intelligently make copies
-- on demand.
--
-- The approach here was inspired by the following blog post by Bryan O'Sullivan:
-- <http://www.serpentine.com/blog/2014/05/31/attoparsec/#bust-my-buffers>
module Data.SnocVector
    ( GSnocVector
    , SnocVector
    , SSnocVector
    , USnocVector
    , toVector
    , fromVector
    , snoc
    , Data.SnocVector.null
    , full
    ) where

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import Control.Monad.Primitive
import Data.ByteString.Internal (inlinePerformIO)
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)
import qualified Data.Vector
import qualified Data.Vector.Storable
import qualified Data.Vector.Unboxed
import Data.Monoid (Monoid (..))
import Data.Atomics.Counter.Unboxed
import GHC.Prim
import GHC.IO (IO (..))

data GSnocVector v a = GSnocVector
    { _svGenMut :: {-# UNPACK #-} !AtomicCounter
    , _svVector :: V.Mutable v RealWorld a
    , _svGen    :: {-# UNPACK #-} !CTicket
    , _svUsed   :: {-# UNPACK #-} !Int
    , _svState  :: State# RealWorld
    }

snoc :: V.Vector v a
     => GSnocVector v a
     -> a -- ^ value to snoc
     -> GSnocVector v a
snoc (GSnocVector counter vm currGen currUsed s#) value =
    g s'#
  where
    (# s'#, g #) = f s#
    IO f = if currUsed >= len
        then expand
        else do
            (success, currGen') <- casCounter counter currGen (succ (peekCTicket currGen))
            if success
                then write (peekCTicket currGen')
                else copy

    len = VM.length vm

    expand = do
        vm' <- VM.grow vm $ max 1024 len
        VM.unsafeWrite vm' currUsed value
        let gen = succ currGen
            used = succ currUsed
        s' <- newCounter gen
        return (GSnocVector s' vm' gen used)

    write gen = do
        VM.unsafeWrite vm currUsed value
        return (GSnocVector counter vm gen (succ currUsed))
    copy = do
        vm' <- VM.clone vm
        VM.unsafeWrite vm' currUsed value
        let used = succ currUsed
        s' <- newCounter 0
        return (GSnocVector s' vm' (succ currGen) used)
{-# INLINE snoc #-}

type SnocVector = GSnocVector Data.Vector.Vector
type SSnocVector = GSnocVector Data.Vector.Storable.Vector
type USnocVector = GSnocVector Data.Vector.Unboxed.Vector

null :: GSnocVector v a -> Bool
null (GSnocVector _ _ _ 0 _) = True
null _ = False
{-# INLINE null #-}

full :: V.Vector v a => GSnocVector v a -> Bool
full (GSnocVector _ vm _ used _) = VM.length vm == used
{-# INLINE full #-}

instance V.Vector v a => Monoid (GSnocVector v a) where
    mempty = unsafeDupablePerformIO $ do
        counter <- newCounter 0
        vm <- VM.new 1024
        IO $ \s# ->
            (# s#
            , GSnocVector counter vm 0 0 s# #)
    {-# INLINE mempty #-}
    mappend sv1 sv2 = V.foldl' snoc sv1 (toVector sv2)
    {-# INLINE mappend #-}

toVector :: V.Vector v a => GSnocVector v a -> v a
toVector (GSnocVector _ mv _ used _) = unsafeDupablePerformIO $ do
    v <- V.unsafeFreeze mv
    return $! V.unsafeTake used v
{-# INLINE toVector #-}

fromVector :: V.Vector v a => v a -> GSnocVector v a
fromVector v = unsafeDupablePerformIO $ do
    mv <- V.unsafeThaw v
    s <- newCounter 0
    IO $ \s# ->
        (# s#
        , GSnocVector s (VM.unsafeTake (V.length v) mv) 0 (V.length v)  s# #)
{-# INLINE fromVector #-}
