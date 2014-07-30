-- | This benchmark tests the ideal case for our library: only appending,
-- never reusing an old buffer.
import Criterion.Main
import qualified Data.SnocVector as SV
import qualified Data.SnocVector.Lazy as LSV
import Data.Monoid (mempty)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.DeepSeq (NFData, ($!!))
import qualified Data.Sequence as Seq
import Data.IORef (newIORef, readIORef)
import qualified Data.List as L
import qualified Data.Foldable as F

mkBench :: NFData b
        => String
        -> a
        -> (a -> b)
        -> (a -> Int -> a)
        -> ((Int -> Int -> Int) -> Int -> b -> Int)
        -> Benchmark
mkBench name empty finish snoc fold =
    bench name $ whnfIO (loop empty 10000)
  where
    loop x 0 = do
        ref <- newIORef $!! finish x
        val <- readIORef ref
        return $! fold (+) 0 val
    loop x i = loop (snoc x i) (pred i)

main :: IO ()
main = defaultMain
    [ mkBench "diff" id ($ []) (\xs x -> xs . (x:)) L.foldl'
    , mkBench "Seq" mempty id (Seq.|>) F.foldl'
    , mkBench "Vector" mempty id V.snoc V.foldl'
    , mkBench "UVector" mempty id VU.snoc VU.foldl'
    , mkBench "SnocVector" (mempty :: SV.SnocVector Int) SV.toVector SV.snoc V.foldl'
    , mkBench "USnocVector" (mempty :: SV.USnocVector Int) SV.toVector SV.snoc VU.foldl'
    --, mkBench "LUSnocVector" (mempty :: LSV.LUSnocVector Int) LSV.toVectors LSV.snoc
    , mkBench "list" [] id (\xs x -> xs ++ [x]) L.foldl'
    ]
