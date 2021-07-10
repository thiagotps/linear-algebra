module LinearAlgebra.Vector where

import LinearAlgebra.FFI
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Vector.Storable (Vector)
import System.IO.Unsafe (unsafePerformIO)
import Foreign
import Foreign.C.Types

newtype VectorDouble = VectorDouble (ForeignPtr CVectorDouble)

instance Show VectorDouble where
  show vd = "VectorDouble "  ++ show (toList vd)

createVectorDouble :: CInt -> Maybe (VS.Vector CDouble) -> IO VectorDouble
createVectorDouble n (Just v) = do
  VS.unsafeWith v $ \vp -> do
    ptr <- c_new_vector n vp
    VectorDouble <$> newForeignPtr c_free_vector ptr
createVectorDouble n Nothing = do
    ptr <- c_new_vector n nullPtr
    VectorDouble <$> newForeignPtr c_free_vector ptr

toVectorIO :: VectorDouble -> IO (VS.Vector Double)
toVectorIO (VectorDouble fPtr) = do
  withForeignPtr fPtr $ \p -> do
    n <- fromC <$> c_vector_size p
    v <- VSM.new n
    VSM.unsafeWith v $ \vp -> c_vector_to_list p vp
    VS.map fromC <$> VS.unsafeFreeze v

toVector :: VectorDouble -> VS.Vector Double
toVector = unsafePerformIO . toVectorIO

fromVector :: VS.Vector Double -> VectorDouble
fromVector v = unsafePerformIO $ createVectorDouble (toC . VS.length $ v) (Just . VS.map toC $ v)

fromList :: [Double] -> VectorDouble
fromList = fromVector . VS.fromList

toList :: VectorDouble -> [Double]
toList = VS.toList . toVector

dot :: VectorDouble -> VectorDouble -> Double
dot (VectorDouble a) (VectorDouble b) = unsafePerformIO $ do
  withForeignPtr a $ \aptr ->
    withForeignPtr b $ \bptr ->
    fromC <$> c_dot aptr bptr

add :: VectorDouble -> VectorDouble -> VectorDouble
add (VectorDouble a) (VectorDouble b) = unsafePerformIO $ do
  withForeignPtr a $ \ap ->
    withForeignPtr b $ \bp -> do
        n <- c_vector_size bp
        dest@(VectorDouble c) <- createVectorDouble n Nothing
        withForeignPtr c $ \cp -> c_add ap bp cp
        return dest

(!) :: VectorDouble -> Int -> Double
(!) (VectorDouble a) n = fromC . unsafePerformIO $ withForeignPtr a (`c_get_vector_elem` toC n)
