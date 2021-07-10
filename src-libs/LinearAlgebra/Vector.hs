{-# LANGUAGE TypeFamilies #-}
module LinearAlgebra.Vector where

import LinearAlgebra.FFI
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Vector.Storable (Vector)
import System.IO.Unsafe (unsafePerformIO)
import Foreign
import Foreign.C.Types
import Control.Monad

newtype VectorDouble = VectorDouble (ForeignPtr CVectorDouble)

instance Show VectorDouble where
  show vd = "VectorDouble "  ++ show (toList vd)

instance IsPtr VectorDouble where
  type PtrType VectorDouble = CVectorDouble
  asPtr (VectorDouble fptr) = withForeignPtr fptr

createVectorDouble :: VS.Vector CDouble -> IO VectorDouble
createVectorDouble v = do
  let n = toC . VS.length $ v
  VS.unsafeWith v $ \vp -> do
    ptr <- c_new_vector n vp
    VectorDouble <$> newForeignPtr c_free_vector ptr

toVectorIO :: VectorDouble -> IO (VS.Vector Double)
toVectorIO v = do
  asPtr v $ \p -> do
    n <- fromC <$> c_vector_size p
    v <- VSM.new n
    VSM.unsafeWith v $ \vp -> c_vector_to_list p vp
    VS.map fromC <$> VS.unsafeFreeze v

toVector :: VectorDouble -> VS.Vector Double
toVector = unsafePerformIO . toVectorIO

fromVector :: VS.Vector Double -> VectorDouble
fromVector v = unsafePerformIO $ createVectorDouble . VS.map toC  $ v

fromList :: [Double] -> VectorDouble
fromList = fromVector . VS.fromList

toList :: VectorDouble -> [Double]
toList = VS.toList . toVector

infixl 7 .*.
(.*.) = dot
dot :: VectorDouble -> VectorDouble -> Double
dot a b = unsafePerformIO $ do
  asPtr a $ \aptr ->
    asPtr b (fmap fromC . c_dot aptr)

vectorDoubleFromCPtr :: Ptr CVectorDouble -> IO VectorDouble
vectorDoubleFromCPtr ptr = VectorDouble <$> newForeignPtr c_free_vector ptr



infixl 7 .+.
(.+.) = add
add :: VectorDouble -> VectorDouble -> VectorDouble
add a b = unsafePerformIO $ do
  asPtr a $ \ap ->
    asPtr b (c_add ap >=> vectorDoubleFromCPtr)

mulScalar :: Double -> VectorDouble -> VectorDouble
mulScalar s v = unsafePerformIO $ asPtr v (c_mul_vector_by_scalar (toC s) >=> vectorDoubleFromCPtr)

norm :: VectorDouble -> Double
norm v = sqrt (dot v v)

normalize :: VectorDouble -> VectorDouble
normalize v = mulScalar (1/norm v) v

(!) :: VectorDouble -> Int -> Double
(!) (VectorDouble a) n = fromC . unsafePerformIO $ withForeignPtr a (`c_get_vector_elem` toC n)

length :: VectorDouble -> Int
length v = unsafePerformIO $ asPtr v (fmap fromC . c_vector_size)
