module Main where

import Foreign.C.Types
import qualified Sparse as S
import qualified Data.Vector.Storable as VS

main = do
  let v = S.fromList 3 3 [(0,0,1), (1,1,2), (2,2,3)]
  print v
