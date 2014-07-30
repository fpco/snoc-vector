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

data Buffer vm a = Buffer
    { _bufferState :: {-# UNPACK #-} !(IORef BuffState) -- generation number, elements used
    , _bufferVector :: !(vm RealWorld a)
    }

newBuffer :: VM.MVector vm a => IO (Buffer vm a)
newBuffer = liftM2 Buffer
    (newIORef (BuffState 0 0))
    (VM.new 1024)

snocBuffer :: VM.MVector vm a
           => Int -- ^ current generation
           -> Int -- ^ used
           -> a -- ^ value to snoc
           -> Buffer vm a
           -> IO (Buffer vm a, Int)
snocBuffer currGen currUsed value buff@(Buffer s vm) = do
    join $ atomicModifyIORef' s $ \origBuff@(BuffState buffGen buffUsed) ->
        case assert (currUsed <= buffUsed) () of
            ()
                | buffUsed >= len -> (origBuff, expand)
                | currGen == buffGen && currUsed == buffUsed -> (BuffState (succ buffGen) (succ buffUsed), write)
                | otherwise -> (origBuff, copy)
  where
    len = VM.length vm

    expand = do
        vm' <- VM.grow vm $ max 1024 len
        VM.write vm' currUsed value
        let gen = succ currGen
        s' <- newIORef $! BuffState gen $! succ currUsed
        return (Buffer s' vm', gen)

    write = do
        VM.write vm currUsed value
        return (buff, succ currGen)
    copy = do
        vm' <- VM.clone vm
        VM.write vm' currUsed value
        s' <- newIORef $! BuffState 0 $! succ currUsed
        return (Buffer s' vm', 0)

data GSnocVector v a = SnocVector
    { _svBuffer :: {-# UNPACK #-} !(Buffer (V.Mutable v) a)
    , _svGen    :: {-# UNPACK #-} !Int
    , _svUsed   :: {-# UNPACK #-} !Int
    }

type SnocVector = GSnocVector Data.Vector.Vector
type SSnocVector = GSnocVector Data.Vector.Storable.Vector
type USnocVector = GSnocVector Data.Vector.Unboxed.Vector

null :: GSnocVector v a -> Bool
null (SnocVector _ _ 0) = True
null _ = False

full :: V.Vector v a => GSnocVector v a -> Bool
full (SnocVector (Buffer _ vm) _ used) = VM.length vm == used

instance V.Vector v a => Monoid (GSnocVector v a) where
    mempty = unsafePerformIO $ do
        buff <- newBuffer
        return $! SnocVector buff 0 0
    mappend sv1 sv2 = V.foldl' snoc sv1 (toVector sv2)

snoc :: V.Vector v a => GSnocVector v a -> a -> GSnocVector v a
snoc (SnocVector buffer gen used) value = unsafePerformIO $ do
    (buffer', gen') <- snocBuffer gen used value buffer
    return $! SnocVector buffer' gen' (succ used)

toVector :: V.Vector v a => GSnocVector v a -> v a
toVector (SnocVector (Buffer _ mv) _ used) = unsafePerformIO $ do
    v <- V.unsafeFreeze mv
    return $! V.unsafeTake used v

fromVector :: V.Vector v a => v a -> GSnocVector v a
fromVector v = unsafePerformIO $ do
    mv <- V.unsafeThaw v
    s <- newIORef $! BuffState 0 $! VM.length mv
    return (SnocVector (Buffer s mv) 0 (V.length v))
