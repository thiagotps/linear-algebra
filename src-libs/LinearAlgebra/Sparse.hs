{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
module LinearAlgebra.Sparse (module LinearAlgebra.Sparse) where
import LinearAlgebra.FFI
import qualified LinearAlgebra.Vector as V
import LinearAlgebra.Vector (VectorDouble(..), vectorDoubleFromCPtr, (.*.))
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Vector.Storable (Vector)
import System.IO.Unsafe (unsafePerformIO)
import Foreign
import Foreign.C.Types
import Control.Monad
import qualified Data.List as L
import GHC.Read (readPrec)
import Control.Arrow (first)

newtype SparseMatrix = SparseMatrix (ForeignPtr CSparseMatrix)

instance IsPtr SparseMatrix where
  type PtrType SparseMatrix = CSparseMatrix
  asPtr (SparseMatrix ptr) = withForeignPtr ptr

instance Show SparseMatrix where
  show s = "SparseMatrix " ++ show (m, n, l)
    where
      l = toList s
      m = rows s
      n = cols s

instance Read SparseMatrix where
  readsPrec s = map (first go) . readsPrec s
    where
      go (m, n, l) = fromList m n l

sparseMatrixFromC :: Ptr CSparseMatrix -> IO SparseMatrix
sparseMatrixFromC ptr = SparseMatrix <$> newForeignPtr c_free_sparse_matrix ptr

createSparseMatrix :: CInt -> CInt -> VS.Vector CTriplet -> IO SparseMatrix
createSparseMatrix nrow ncol v = do
  let n = toC . VS.length $ v
  VS.unsafeWith v $ \vPtr -> do
    ptr <- c_new_sparse_matrix nrow ncol vPtr n
    if ptr /= nullPtr
      then sparseMatrixFromC ptr
      else
        error "It was not possible to create a SparseMatrix. Allocation error. This is a severe error !"

toCVector :: SparseMatrix -> IO (VS.Vector CTriplet)
toCVector m = do
  asPtr m $ \p -> do
    s <- c_non_zeros p
    tris <- VSM.new (fromC s)
    VSM.unsafeWith tris $ \q ->
      c_sparse_to_list p q s
    VS.unsafeFreeze tris

fromList :: Int -> Int -> [(Int, Int, Double)] -> SparseMatrix
fromList nrow ncol l = unsafePerformIO $ createSparseMatrix (toC nrow) (toC ncol) (VS.fromList . map toC $ l)

toList :: SparseMatrix -> [(Int, Int, Double)]
toList = map fromC . VS.toList . unsafePerformIO . toCVector

infixl 7 !*.
(!*.) = mul
mul :: SparseMatrix -> VectorDouble -> VectorDouble
mul a b = unsafePerformIO $ do
  asPtr a $ \ap ->
    asPtr b (c_mul ap >=> vectorDoubleFromCPtr)

rows :: SparseMatrix -> Int
rows sm = unsafePerformIO $ asPtr sm (fmap fromC . c_sparse_rows)

cols :: SparseMatrix -> Int
cols sm = unsafePerformIO $ asPtr sm (fmap fromC . c_sparse_cols)

data EigenConfig = EigenConfig {ncv :: Int, iterations :: Int, precision :: Double} deriving (Eq, Show, Read)
data CompInfo=  Successful | NotComputed | NotConverging | NumericalIssue deriving (Eq, Show, Read, Enum)

eigenDefaultConfig = EigenConfig{ncv=2*nev + 1, iterations=2000, precision=10**(-5)}
  where
    nev = 1 -- Number of eigenvalues

maxEigenValue :: EigenConfig -> SparseMatrix -> Either CompInfo Double
maxEigenValue EigenConfig{ncv, iterations, precision} a = unsafePerformIO $ do
  asPtr a $ \ap -> do
    alloca $ \dest -> do
      r <- toEnum . fromC <$> c_largest_eigen_value ap (toC ncv) (toC iterations) (toC precision) dest
      if r == Successful
        then Right . fromC <$> peek dest
        else return (Left r)

powerMethodIteration :: SparseMatrix -> [Double]
powerMethodIteration a = eigen
  where
    n = rows a
    b0 = V.fromVector $ VS.generate n (const 1)
    b = b0 : [ V.normalize bk | bkl1 <- b, let bk = a !*. bkl1]
    eigen = [(bk .*. (a !*. bk)) / (bk .*. bk) | bk <- tail b]

powerMaxEigenValue' :: Double -> Int -> SparseMatrix -> Maybe Double
powerMaxEigenValue' precision maxIterNumber a = fmap snd . L.find (\(f,s) -> abs(s - f) <= abs f * precision) . take maxIterNumber $ zip eigenList (tail eigenList)
  where
    eigenList = powerMethodIteration a

powerMaxEigenValue = powerMaxEigenValue' (10 ** (-5)) (10^4)
