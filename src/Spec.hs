{-# LANGUAGE ScopedTypeVariables #-}
import Data.SnocVector
import qualified Data.Vector
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.List (foldl')
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Monoid (Monoid (..))

data TestSnoc a = TestSnoc ![a] !(SnocVector a)

testEmpty :: TestSnoc a
testEmpty = TestSnoc [] mempty

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