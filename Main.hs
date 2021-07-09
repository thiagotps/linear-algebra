module Main where

import Foreign.C.Types
import LinearAlgebra.Sparse as S
import LinearAlgebra.Vector as V
import qualified Data.Vector.Storable as VS

infixl 7 !*.
infixl 7 .*.
infixl 7 .+.

(!*.) = S.mul
(.*.) = V.dot
(.+.) = V.add

main = do
  let m = S.fromList 3 3 [(0,0,2), (1,1,2), (2,2,2)]
  let a = V.fromList [0,1,2]
  let b = V.fromList [3,4,5]
  print m
  print (a .*. b)
  print (m !*. a .+. b)
