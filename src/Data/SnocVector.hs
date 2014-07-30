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

import Data.IORef
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import Control.Monad (liftM2, join)
import Control.Monad.Primitive
import Control.Exception
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector
import qualified Data.Vector.Storable
import qualified Data.Vector.Unboxed
import Data.Monoid (Monoid (..))

data BuffState = BuffState
    { _bsGen :: {-# UNPACK #-} !Int
    , _bsUsed :: {-# UNPACK #-} !Int
    }

data GSnocVector v a = GSnocVector
    { _svState  :: {-# UNPACK #-} !(IORef BuffState)
    , _svVector :: V.Mutable v RealWorld a
    , _svGen    :: {-# UNPACK #-} !Int
    , _svUsed   :: {-# UNPACK #-} !Int
    }

snoc :: V.Vector v a
     => GSnocVector v a
     -> a -- ^ value to snoc
     -> GSnocVector v a
snoc (GSnocVector s vm currGen currUsed) value = unsafePerformIO $ join $
    atomicModifyIORef' s $ \origBuff@(BuffState buffGen buffUsed) ->
        case assert (currUsed <= buffUsed) () of
          ()
            | buffUsed >= len -> (origBuff, expand)
            | currGen == buffGen && currUsed == buffUsed ->
                let gen = succ buffGen
                 in (BuffState gen (succ buffUsed), write gen)
            | otherwise -> (origBuff, copy)
  where
    len = VM.length vm

    expand = do
        vm' <- VM.grow vm $ max 1024 len
        VM.write vm' currUsed value
        let gen = succ currGen
        s' <- newIORef $! BuffState gen $! succ currUsed
        return $! GSnocVector s' vm' gen (succ currUsed)

    write gen = do
        VM.write vm currUsed value
        return $! GSnocVector s vm gen (succ currUsed)
    copy = do
        vm' <- VM.clone vm
        VM.write vm' currUsed value
        let used = succ currUsed
        s' <- newIORef $! BuffState 0 used
        return $! GSnocVector s' vm' (succ currGen) used
{-# INLINE snoc #-}

type SnocVector = GSnocVector Data.Vector.Vector
type SSnocVector = GSnocVector Data.Vector.Storable.Vector
type USnocVector = GSnocVector Data.Vector.Unboxed.Vector

null :: GSnocVector v a -> Bool
null (GSnocVector _ _ _ 0) = True
null _ = False
{-# INLINE null #-}

full :: V.Vector v a => GSnocVector v a -> Bool
full (GSnocVector _ vm _ used) = VM.length vm == used
{-# INLINE full #-}

instance V.Vector v a => Monoid (GSnocVector v a) where
    mempty = unsafePerformIO $ do
        bs <- newIORef (BuffState 0 0)
        vm <- VM.new 1024
        return $! GSnocVector bs vm 0 0
    mappend sv1 sv2 = V.foldl' snoc sv1 (toVector sv2)

toVector :: V.Vector v a => GSnocVector v a -> v a
toVector (GSnocVector _ mv _ used) = unsafePerformIO $ do
    v <- V.unsafeFreeze mv
    return $! V.unsafeTake used v
{-# INLINE toVector #-}

fromVector :: V.Vector v a => v a -> GSnocVector v a
fromVector v = unsafePerformIO $ do
    mv <- V.unsafeThaw v
    s <- newIORef $! BuffState 0 $! VM.length mv
    return $! GSnocVector s mv 0 (V.length v)
{-# INLINE fromVector #-}
