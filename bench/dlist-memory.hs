import Control.DeepSeq

loop front 0 = front []
loop front i = loop (front . (i:)) (pred i)

main :: IO ()
main = do
    let list = loop id (1000000 :: Int)
    return $!! length list
    return $!! length list
    return ()
