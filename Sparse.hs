module Sparse (module Sparse) where
import FFI
import qualified Data.Vector.Unboxed as VB
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (Vector)
import System.IO.Unsafe (unsafePerformIO)
import Foreign

newtype SparseMatrix = SparseMatrix (ForeignPtr CSparseMatrix)

instance Show SparseMatrix where
  show s = "SparseMatrix " ++ show (toList s)

fromList :: Int -> Int -> [(Int, Int, Double)] -> SparseMatrix
fromList nrow ncol l = SparseMatrix . unsafePerformIO $ createCSparseMatrix (toC nrow) (toC ncol) (VS.fromList . map toC $ l)

toList :: SparseMatrix -> [(Int, Int, Double)]
toList (SparseMatrix fPtr) = map fromC . VS.toList . unsafePerformIO . toCVector $ fPtr
