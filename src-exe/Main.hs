module Main where

import Foreign.C.Types
import LinearAlgebra.Sparse (SparseMatrix, EigenConfig(..), (!*.), eigenDefaultConfig)
import qualified LinearAlgebra.Sparse as S
import LinearAlgebra.Vector (VectorDouble, (.*.), (.+.))
import qualified LinearAlgebra.Vector as V
import qualified Data.Vector.Storable as VS
import System.IO

newtype Sparse = Sparse {getSparse :: [[(Int, Double)]]} deriving (Eq, Show, Read)

readMatrix :: String -> SparseMatrix
readMatrix str = S.fromList m n l
  where
    (m, n, l) = read . unwords . tail . words $ str

main = do
  putStrLn "Getting matrix..."
  withFile "lmshs.log" ReadMode $ \h -> do
    m <- readMatrix <$> hGetContents h
    putStrLn "Searching maximum eigenvalue..."
    -- putStrLn $ "maxEigenValuePower = " ++ show (S.powerMaxEigenValue m)
    putStrLn $ "maxEigenValue = " ++ show (S.maxEigenValue eigenDefaultConfig{ncv= ncv eigenDefaultConfig + 4} m)


-- main = do
--   let m = S.fromList 3 3 [(0,0,2), (1,1,2), (2,2,2)]
--   let a = V.fromList [0,1,2]
--   let b = V.fromList [3,4,5]
--   print m
--   print (a .*. b)
--   print (m !*. a .+. b)
--   putStrLn $ "a's max eigenvalue is " ++ show (S.maxEigenValue m)
--   putStrLn $ "The 1-th element of a is " ++ show (a V.! 1)
