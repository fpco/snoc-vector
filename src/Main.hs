{-# LANGUAGE ScopedTypeVariables #-}
import Data.Primitive.MutVar
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM
import Data.Word (Word)
import Control.Monad (liftM2, join)
import Control.Monad.Primitive
import Control.Exception
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.List (foldl')
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Monoid (Monoid (..))

data Buffer vm s a = Buffer
    { bufferState :: {-# UNPACK #-} !(MutVar s (Word, Word)) -- generation number, elements used
    , bufferVector :: !(vm s a)
    }

newBuffer :: (VM.MVector vm a, PrimMonad m) => m (Buffer vm (PrimState m) a)
newBuffer = liftM2 Buffer
    (newMutVar (0, 0))
    (VM.new 16)

snocBuffer :: (PrimMonad m, VM.MVector vm a)
           => Word -- ^ current generation
           -> Word -- ^ used
           -> a -- ^ value to snoc
           -> Buffer vm (PrimState m) a
           -> m (Buffer vm (PrimState m) a, Word)
snocBuffer currGen currUsed value buff@(Buffer s vm) = do
    join $ atomicModifyMutVar' s $ \(buffGen, buffUsed) ->
        case assert (currUsed <= buffUsed) () of
            ()
                | buffUsed >= fromIntegral len -> ((buffGen, buffUsed), expand)
                | currGen == buffGen && currUsed == buffUsed -> ((succ buffGen, succ buffUsed), write)
                | otherwise -> ((buffGen, buffUsed), copy)
  where
    len = VM.length vm

    expand = do
        vm' <- VM.grow vm $ max 8 len
        VM.write vm' (fromIntegral currUsed) value
        s' <- newMutVar (0, succ currUsed)
        return (Buffer s' vm', 0)

    write = do
        VM.write vm (fromIntegral currUsed) value
        return (buff, succ currGen)
    copy = do
        vm' <- VM.clone vm
        VM.write vm' (fromIntegral currUsed) value
        s' <- newMutVar (0, succ currUsed)
        return (Buffer s' vm', 0)

data SnocVector v a = SnocVector
    { svBuffer :: Buffer (V.Mutable v) RealWorld a
    , svGen    :: {-# UNPACK #-} !Word
    , svUsed   :: {-# UNPACK #-} !Word
    }

instance V.Vector v a => Monoid (SnocVector v a) where
    mempty = empty
    mappend sv1 sv2 = V.foldl' snoc sv1 (toVector sv2)

empty :: V.Vector v a => SnocVector v a
empty = unsafePerformIO $ do
    buff <- newBuffer
    return $! SnocVector buff 0 0

snoc :: V.Vector v a => SnocVector v a -> a -> SnocVector v a
snoc (SnocVector buffer gen used) value = unsafePerformIO $ do
    (buffer', gen') <- snocBuffer gen used value buffer
    return $! SnocVector buffer' gen' (succ used)

toVector :: V.Vector v a => SnocVector v a -> v a
toVector (SnocVector (Buffer _ mv) _ used) = unsafePerformIO $ do
    v <- V.unsafeFreeze mv
    return $! V.unsafeTake (fromIntegral used) v

fromVector :: V.Vector v a => v a -> SnocVector v a
fromVector v = unsafePerformIO $ do
    mv <- V.unsafeThaw v
    s <- newMutVar (0, fromIntegral $ VM.length mv)
    return (SnocVector (Buffer s mv) 0 (fromIntegral $ V.length v))

data TestSnoc a = TestSnoc [a] (SnocVector Data.Vector.Vector a)

testEmpty :: TestSnoc a
testEmpty = TestSnoc [] empty

testSnoc :: (Eq a, Show a) => TestSnoc a -> a -> TestSnoc a
testSnoc (TestSnoc x y) a
    | Data.Vector.toList (toVector y') == x' = TestSnoc x' y'
    | otherwise = error $ "testSnoc failed: " ++ show (x', toVector y')
  where
    x' = x ++ [a]
    y' = snoc y a

main :: IO ()
main = hspec $ do
    prop "snoc" $ \actions ->
        let res = foldl' step [] $ concat $ replicate 20 actions
            step stack (val, toDrop') =
                curr `seq` drop toDrop (new : stack)
              where
                curr = fromMaybe testEmpty $ listToMaybe stack
                new = testSnoc curr (val :: Int)
                toDrop = max 0 toDrop'
         in res `seq` True
    prop "toVector/fromVector mappend" $ \(x1 :: [Int], x2) -> do
        let v1 = Data.Vector.fromList x1
            sv1 = fromVector v1
            v2 = Data.Vector.fromList x2
            sv2 = fromVector v2
            v = mappend v1 v2
            sv = mappend sv1 sv2
         in toVector sv == v