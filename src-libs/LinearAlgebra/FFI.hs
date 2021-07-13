{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module LinearAlgebra.FFI (module LinearAlgebra.FFI) where
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import GHC.IO (unsafePerformIO)
import Foreign.Marshal

class Cast a where
  type C a
  toC :: a -> C a
  fromC :: C a -> a

instance Cast Int where
  type C Int = CInt
  toC = fromIntegral
  fromC (CInt n) = fromIntegral n

instance Cast Double where
  type C Double = CDouble
  toC = realToFrac
  fromC (CDouble n) = realToFrac n

instance Cast (Int, Int, Double) where
  type C (Int, Int, Double) = CTriplet
  toC (n,m,a) = CTriplet (toC n) (toC m) (toC a)
  fromC CTriplet{mRow, mCol, mValue} = (fromC mRow, fromC mCol, fromC mValue)

data CTriplet = CTriplet {mRow :: !CInt, mCol :: !CInt, mValue :: !CDouble} deriving (Show)

instance  Storable CTriplet where
    sizeOf _ = sizeOf (undefined :: CDouble) + sizeOf (undefined :: CInt) * 2
    alignment _ = alignment (undefined :: CInt)
    poke p (CTriplet row col val) = do
        pokeElemOff (castPtr p) 0 row
        pokeElemOff (castPtr p) 1 col
        pokeByteOff p (sizeOf (undefined :: CInt) * 2) val
    peek p = CTriplet
        <$> peekElemOff (castPtr p) 0
        <*> peekElemOff (castPtr p) 1
        <*> peekByteOff p (sizeOf (undefined :: CInt) * 2)

class IsPtr m where
  type family PtrType m
  asPtr :: m -> (Ptr (PtrType m) -> IO b) -> IO b

data CSparseMatrix
data CVectorDouble


foreign import ccall unsafe "teste.c new_sparse_matrix" c_new_sparse_matrix :: CInt -> CInt -> Ptr CTriplet -> CInt -> IO (Ptr CSparseMatrix)
foreign import ccall unsafe "teste.c &free_sparse_matrix" c_free_sparse_matrix :: FunPtr(Ptr CSparseMatrix -> IO ())
foreign import ccall unsafe "teste.c non_zeros" c_non_zeros :: Ptr CSparseMatrix -> IO CInt
foreign import ccall unsafe "teste.c sparse_to_list" c_sparse_to_list :: Ptr CSparseMatrix -> Ptr CTriplet -> CInt -> IO CInt
foreign import ccall unsafe "teste.c new_vector" c_new_vector :: CInt -> Ptr CDouble -> IO (Ptr CVectorDouble)
foreign import ccall unsafe "teste.c &free_vector" c_free_vector :: FunPtr (Ptr CVectorDouble -> IO ())

foreign import ccall unsafe "teste.c sparse_rows" c_sparse_rows :: Ptr CSparseMatrix -> IO CInt
foreign import ccall unsafe "teste.c sparse_cols" c_sparse_cols :: Ptr CSparseMatrix -> IO CInt

foreign import ccall unsafe "teste.c dot" c_dot :: Ptr CVectorDouble -> Ptr CVectorDouble -> IO CDouble
foreign import ccall unsafe "teste.c mul" c_mul :: Ptr CSparseMatrix -> Ptr CVectorDouble -> IO (Ptr CVectorDouble)
foreign import ccall unsafe "teste.c add " c_add :: Ptr CVectorDouble -> Ptr CVectorDouble -> IO (Ptr CVectorDouble)
foreign import ccall unsafe "teste.c mul_vector_by_scalar " c_mul_vector_by_scalar :: CDouble -> Ptr CVectorDouble -> IO (Ptr CVectorDouble)

foreign import ccall unsafe "teste.c vector_size " c_vector_size :: Ptr CVectorDouble -> IO CInt
foreign import ccall unsafe "teste.c vector_to_list " c_vector_to_list :: Ptr CVectorDouble -> Ptr CDouble ->  IO ()

foreign import ccall unsafe "teste.c get_vector_elem" c_get_vector_elem :: Ptr CVectorDouble -> CInt ->  IO CDouble
foreign import ccall unsafe "teste.c largest_eigen_value"  c_largest_eigen_value :: Ptr CSparseMatrix -> CInt -> CInt -> CDouble -> Ptr CDouble ->  IO CInt
