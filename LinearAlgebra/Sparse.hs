module LinearAlgebra.Sparse (module LinearAlgebra.Sparse) where
import LinearAlgebra.FFI
import qualified LinearAlgebra.Vector as V
import LinearAlgebra.Vector (VectorDouble(..))
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Vector.Storable (Vector)
import System.IO.Unsafe (unsafePerformIO)
import Foreign
import Foreign.C.Types

newtype SparseMatrix = SparseMatrix (ForeignPtr CSparseMatrix)

instance Show SparseMatrix where
  show s = "SparseMatrix " ++ show (toList s)

createSparseMatrix :: CInt -> CInt -> VS.Vector CTriplet -> IO SparseMatrix
createSparseMatrix nrow ncol v = do
  let n = toC . VS.length $ v
  VS.unsafeWith v $ \vPtr -> do
    ptr <- c_new_sparse_matrix nrow ncol vPtr n
    if ptr /= nullPtr
      then SparseMatrix <$> newForeignPtr c_free_sparse_matrix ptr
      else
        error "It was not possible to create a SparseMatrix. Allocation error. This is a severe error !"

toCVector :: SparseMatrix -> IO (VS.Vector CTriplet)
toCVector (SparseMatrix fPtr) = do
  withForeignPtr fPtr $ \p -> do
    s <- c_non_zeros p
    tris <- VSM.new (fromC s)
    VSM.unsafeWith tris $ \q ->
      c_sparse_to_list p q s
    VS.unsafeFreeze tris

fromList :: Int -> Int -> [(Int, Int, Double)] -> SparseMatrix
fromList nrow ncol l = unsafePerformIO $ createSparseMatrix (toC nrow) (toC ncol) (VS.fromList . map toC $ l)

toList :: SparseMatrix -> [(Int, Int, Double)]
toList = map fromC . VS.toList . unsafePerformIO . toCVector

mul :: SparseMatrix -> VectorDouble -> VectorDouble
mul (SparseMatrix a) (VectorDouble b) = unsafePerformIO $ do
  withForeignPtr a $ \ap ->
    withForeignPtr b $ \bp -> do
      n <- c_vector_size bp
      dest@(VectorDouble c) <- V.createVectorDouble n Nothing -- NOTE: This is not safe
      withForeignPtr c $ \cp -> c_mul ap bp cp
      return dest


maxEigenValue :: SparseMatrix -> Maybe Double
maxEigenValue (SparseMatrix a) = unsafePerformIO $ do
  withForeignPtr a $ \ap -> do
    alloca $ \dest -> do
      r <- c_largest_eigen_value ap dest
      if r == 0
        then Just . fromC <$> peek dest
        else return Nothing
