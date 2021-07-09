{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module FFI where
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

data CSparseMatrix

createCSparseMatrix :: CInt -> CInt -> VS.Vector CTriplet -> IO (ForeignPtr CSparseMatrix)
createCSparseMatrix nrow ncol v = do
  let n = toC . VS.length $ v
  VS.unsafeWith v $ \vPtr -> do
    ptr <- c_new_sparse_matrix nrow ncol vPtr n
    if ptr /= nullPtr
      then newForeignPtr c_free_sparse_matrix ptr
      else
        error "It was not possible to create a SparseMatrix. Allocation error. This is a severe error !"

toCVector :: ForeignPtr CSparseMatrix -> IO (VS.Vector CTriplet)
toCVector fPtr = do
  withForeignPtr fPtr $ \p -> do
    s <- c_non_zeros p
    tris <- VSM.new (fromC s)
    VSM.unsafeWith tris $ \q ->
      c_sparse_to_list p q s
    VS.unsafeFreeze tris

data CVectorDouble

createCVectorDouble :: CInt -> Maybe (VS.Vector CDouble) -> IO (ForeignPtr CVectorDouble)
createCVectorDouble n (Just v) = do
  VS.unsafeWith v $ \vp -> do
    ptr <- c_new_vector n vp
    newForeignPtr c_free_vector ptr
createCVectorDouble n Nothing = do
    ptr <- c_new_vector n nullPtr
    newForeignPtr c_free_vector ptr

cVectorDoubleToVector :: ForeignPtr CVectorDouble -> IO (VS.Vector CDouble)
cVectorDoubleToVector fPtr = do
  withForeignPtr fPtr $ \p -> do
    n <- fromC <$> c_vector_size p
    v <- VSM.new n
    VSM.unsafeWith v $ \vp -> c_vector_to_list p vp
    VS.unsafeFreeze v


foreign import ccall unsafe "teste.c new_sparse_matrix" c_new_sparse_matrix :: CInt -> CInt -> Ptr CTriplet -> CInt -> IO (Ptr CSparseMatrix)
foreign import ccall unsafe "teste.c &free_sparse_matrix" c_free_sparse_matrix :: FunPtr(Ptr CSparseMatrix -> IO ())
foreign import ccall unsafe "teste.c non_zeros" c_non_zeros :: Ptr CSparseMatrix -> IO CInt
foreign import ccall unsafe "teste.c sparse_to_list" c_sparse_to_list :: Ptr CSparseMatrix -> Ptr CTriplet -> CInt -> IO CInt
foreign import ccall unsafe "teste.c new_vector" c_new_vector :: CInt -> Ptr CDouble -> IO (Ptr CVectorDouble)
foreign import ccall unsafe "teste.c &free_vector" c_free_vector :: FunPtr (Ptr CVectorDouble -> IO ())

foreign import ccall unsafe "teste.c dot" c_dot :: Ptr CVectorDouble -> Ptr CVectorDouble -> CDouble
foreign import ccall unsafe "teste.c mul" c_mul :: Ptr CSparseMatrix -> Ptr CVectorDouble -> Ptr CVectorDouble -> IO ()
foreign import ccall unsafe "teste.c add " c_add :: Ptr CSparseMatrix -> Ptr CVectorDouble -> Ptr CVectorDouble -> IO ()

foreign import ccall unsafe "teste.c vector_size " c_vector_size :: Ptr CVectorDouble -> IO CInt
foreign import ccall unsafe "teste.c vector_to_list " c_vector_to_list :: Ptr CVectorDouble -> Ptr CDouble ->  IO ()
