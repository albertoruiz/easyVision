module Util.Sparse where

import Numeric.LinearAlgebra
import qualified Data.Map as Map
import qualified Data.Array as A
import Data.List

data MatrixBlock t = Zero
                   | Diagonal [Matrix t]
                   | Dense (Matrix t)
                   deriving Show

data SparseMatrix t = SparseMatrix {
    srows :: Int,
    scols :: Int,
    sat   :: (Int,Int) -> MatrixBlock t }

mkSparse :: [((Int,Int), MatrixBlock t)] -> SparseMatrix t
mkSparse asc = SparseMatrix {srows = r, scols = c, sat = at }
    where at = (A.accumArray (flip const) Zero ((0,0),(r-1,c-1)) asc A.!)
          r = succ . maximum . map (fst.fst) $ asc
          c = succ . maximum . map (snd.fst) $ asc

mkSparse' asc = SparseMatrix {srows = r, scols = c, sat = at }
    where at k = maybe Zero id (Map.lookup k m)
          m = Map.fromList asc
          r = succ . maximum . map fst . Map.keys $ m
          c = succ . maximum . map snd . Map.keys $ m

toDense sp = fromBlocks [[f (sat sp (i,j)) | j<-[0..c-1]] | i<- [0..r-1]]
    where r = srows sp
          c = scols sp
          f Zero = 0
          f (Dense x) = x
          f (Diagonal l) = blockDiag l

spTrans Zero = Zero
spTrans (Dense x) = Dense (trans x)
spTrans (Diagonal l) = Diagonal (map trans l)

spAdd Zero x = x
spAdd x Zero = x
spAdd x y = spew (+) x y


spSub Zero x = -x
spSub x Zero = x
spSub x y = spew (-) x y

spew op (Dense x) (Dense y) = Dense (x `op` y)
spew op (Dense x) (Diagonal l) = Dense (x `op` blockDiag l)
spew op (Diagonal l) (Dense x) = Dense (blockDiag l `op` x)
spew op (Diagonal x) (Diagonal y) = Diagonal (zipWith op x y) -- allow different sizes?

spMul Zero _ = Zero
spMul _ Zero = Zero
spMul (Dense x) (Dense y) = Dense (x<>y)
spMul x@(Dense _) y@(Diagonal _) = spTrans (spMul (spTrans y) (spTrans x))
spMul (Diagonal l) (Dense x) = Dense (blockDiagMul l x)
spMul (Diagonal x) (Diagonal y) = Diagonal (zipWith (<>) x y) -- allow different sizes?

-------------------------------

bTrans (SparseMatrix r c at) = SparseMatrix c r at'
    where at' (i,j) = spTrans (at (j,i))

bMul (SparseMatrix rx cx atx) (SparseMatrix ry cy aty) = mkSparse l where
    l = [((i,j), foldl' spAdd Zero [atx (i,k) `spMul` aty (k,j) | k<-ks])| i<-is, j<-js]
    ks = [0..cx-1]
    is = [0..rx-1]
    js = [0..cy-1]

--------------------------------------------

blockDiag bs = fromBlocks [ [ f i j | i<-ns ] | j<-ns ] where
    ns = [ 0 .. length bs -1]
    f i j | i==j = bs!!i
          | otherwise = 0

blockDiagSolve bs m = blockDiagOp linearSolve bs m

blockDiagSolveLU lus m = blockDiagOpG (rows.fst) luSolve lus m

blockDiagMul bs m = blockDiagOp (<>) bs m

blockDiagOp op bs m = blockDiagOpG rows op bs m

blockDiagOpG fbs op bs m = fromBlocks (map return xs) where
    rs = map fbs bs
    ms = map head $ toBlocks rs [cols m] m
    xs = zipWith op bs ms
