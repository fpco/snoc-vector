-- | This benchmark tests the ideal case for our library: only appending,
-- never reusing an old buffer.
import Criterion.Main
import qualified Data.SnocVector as SV
import Data.Monoid (mempty)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.DeepSeq (NFData)
import qualified Data.Sequence as Seq

mkBench :: NFData b
        => String
        -> a
        -> (a -> b)
        -> (a -> Int -> a)
        -> Benchmark
mkBench name empty finish snoc =
    bench name $ nf (loop empty) 1000
  where
    loop x 0 = finish x
    loop x i = loop (snoc x i) (pred i)

main :: IO ()
main = defaultMain
    [ mkBench "list" [] id (\xs x -> xs ++ [x])
    , mkBench "diff" id ($ []) (\xs x -> xs . (x:))
    , mkBench "Seq" mempty id (Seq.|>)
    , mkBench "Vector" mempty id V.snoc
    , mkBench "UVector" mempty id VU.snoc
    , mkBench "SnocVector" (mempty :: SV.SnocVector Int) SV.toVector SV.snoc
    , mkBench "USnocVector" (mempty :: SV.USnocVector Int) SV.toVector SV.snoc
    ]
