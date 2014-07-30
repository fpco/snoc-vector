module Data.SnocVector.Lazy
    ( LGSnocVector
    , LSnocVector
    , LSSnocVector
    , LUSnocVector
    , toVectors
    --, fromVectors
    , snoc
    ) where

import qualified Data.SnocVector as SV
import Data.Monoid (Monoid (..))
import qualified Data.Vector.Generic as V
import qualified Data.Vector
import qualified Data.Vector.Storable
import qualified Data.Vector.Unboxed

data LGSnocVector v a = LGSnocVector (SV.SnocVector (SV.GSnocVector v a)) (SV.GSnocVector v a)

instance V.Vector v a => Monoid (LGSnocVector v a) where
    mempty = LGSnocVector mempty mempty
    LGSnocVector a b `mappend` LGSnocVector c d =
        LGSnocVector (a' `mappend` c) d
      where
        a'
            | SV.null b = a
            | otherwise = SV.snoc a b

type LSnocVector = LGSnocVector Data.Vector.Vector
type LSSnocVector = LGSnocVector Data.Vector.Storable.Vector
type LUSnocVector = LGSnocVector Data.Vector.Unboxed.Vector

snoc :: V.Vector v a => LGSnocVector v a -> a -> LGSnocVector v a
snoc (LGSnocVector vs v) a
    | SV.full v = LGSnocVector (SV.snoc vs v) (SV.snoc mempty a)
    | otherwise = LGSnocVector vs (SV.snoc v a)

toVectors :: V.Vector v a
          => LGSnocVector v a
          -> Data.Vector.Vector (v a)
toVectors (LGSnocVector xs x)
    | SV.null x = V.map SV.toVector $ SV.toVector xs
    | otherwise = V.map SV.toVector $ SV.toVector $ SV.snoc xs x
