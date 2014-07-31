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
import Control.Exception

mkBench :: NFData b
        => String
        -> a
        -> (a -> b)
        -> (a -> Int -> a)
        -> Benchmark
mkBench name empty finish snoc =
    bench name $ whnfIO (loop empty 10000)
  where
    loop x 0 = evaluate $!! finish x
    loop x i = loop (snoc x i) (pred i)

main :: IO ()
main = defaultMain
    [ mkBench "diff" id ($ []) (\xs x -> xs . (x:))
    , mkBench "USnocVector" (mempty :: SV.USnocVector Int) SV.toVector SV.snoc
    , mkBench "SnocVector" (mempty :: SV.SnocVector Int) SV.toVector SV.snoc
    , mkBench "LUSnocVector" (mempty :: LSV.LUSnocVector Int) LSV.toVectors LSV.snoc
    , mkBench "Seq" mempty id (Seq.|>)
    , mkBench "Vector" mempty id V.snoc
    , mkBench "UVector" mempty id VU.snoc
    , mkBench "list" [] id (\xs x -> xs ++ [x])
    ]
