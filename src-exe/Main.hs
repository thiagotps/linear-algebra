module Main where

import Foreign.C.Types
import LinearAlgebra.Sparse as S
import LinearAlgebra.Vector as V
import qualified Data.Vector.Storable as VS
import System.IO

infixl 7 !*.
infixl 7 .*.
infixl 7 .+.

(!*.) = S.mul
(.*.) = V.dot
(.+.) = V.add

newtype Sparse = Sparse {getSparse :: [[(Int, Double)]]} deriving (Eq, Show, Read)


buildMatrix :: [[(Int, Double)]] -> SparseMatrix
buildMatrix s = S.fromList m n $ concat [map (addIdx i) l | (i,l) <- [0..] `zip` s]
  where
    m = length s
    n = (maximum . map fst . concat $ s) + 1
    addIdx i (a,b) = (i, a, b)

main = do
  putStrLn "Getting matrix..."
  withFile "lmshs.log" ReadMode $ \h -> do
    (Sparse s) <- read <$> hGetContents h
    let m = buildMatrix s
    putStrLn "Searching maximum eigenvalue..."
    putStrLn $ "maxEigenValue = " ++ show (maxEigenValue m)

    -- print . maxEigenValue' (10**(-2)) (10^6) $ s
    -- print . (!! 20) . powerAdvancedIteration  $ s

-- main = do
--   let m = S.fromList 3 3 [(0,0,2), (1,1,2), (2,2,2)]
--   let a = V.fromList [0,1,2]
--   let b = V.fromList [3,4,5]
--   print m
--   print (a .*. b)
--   print (m !*. a .+. b)
--   putStrLn $ "a's max eigenvalue is " ++ show (S.maxEigenValue m)
--   putStrLn $ "The 1-th element of a is " ++ show (a V.! 1)
