import Control.DeepSeq
import Data.SnocVector
import Data.Monoid
import qualified Data.Vector as V

loop front 0 = toVector front
loop front i = loop (snoc front i) (pred i)

main :: IO ()
main = do
    let vector = loop mempty (1000000 :: Int)
    return $!! V.length vector
    return $!! V.length vector
    return ()
