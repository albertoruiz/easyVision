{-# LANGUAGE FlexibleContexts #-}

module Util.Sparse(
    MatrixBlock(..), SparseMatrix(..), SMat,
    mkSparse, toDense,
    bMul,bTrans, blockDiag,
    blockDiagSolveLU
) where

import Numeric.LinearAlgebra hiding (i)
--import qualified Data.Map as Map
import qualified Data.Array as A
import Data.List
import Util.Misc(impossible)

data MatrixBlock t = Zero
                   | Diagonal [Matrix t]
                   | Dense (Matrix t)
                   deriving Show

data SparseMatrix t = SparseMatrix {
    srows :: Int,
    scols :: Int,
    sat   :: (Int,Int) -> MatrixBlock t }

type SMat = SparseMatrix Double

mkSparse :: [((Int,Int), MatrixBlock t)] -> SparseMatrix t
mkSparse asc = SparseMatrix {srows = r, scols = c, sat = at }
    where at = (A.accumArray (flip const) Zero ((0,0),(r-1,c-1)) asc A.!)
          r = succ . maximum . map (fst.fst) $ asc
          c = succ . maximum . map (snd.fst) $ asc

{-
mkSparse' asc = SparseMatrix {srows = r, scols = c, sat = at }
    where at k = maybe Zero id (Map.lookup k m)
          m = Map.fromList asc
          r = succ . maximum . map fst . Map.keys $ m
          c = succ . maximum . map snd . Map.keys $ m
-}

toDense :: (Container Vector t, Num (Vector t))
        => SparseMatrix t -> Matrix t
toDense sp = fromBlocks [[f (sat sp (i,j)) | j<-[0..c-1]] | i<- [0..r-1]]
    where r = srows sp
          c = scols sp
          f Zero = 0
          f (Dense x) = x
          f (Diagonal l) = blockDiag l

spTrans :: MatrixBlock t -> MatrixBlock t
spTrans Zero = Zero
spTrans (Dense x) = Dense (trans x)
spTrans (Diagonal l) = Diagonal (map trans l)

spAdd :: (Container Vector t, Num (Vector t))
      => MatrixBlock t -> MatrixBlock t -> MatrixBlock t
spAdd Zero x = x
spAdd x Zero = x
spAdd x y = spew (+) x y

{-
spSub Zero x = -x
spSub x Zero = x
spSub x y = spew (-) x y
-}

spew :: (Container Vector t1, Num (Vector t1), Container Vector t, Num (Vector t))
     => (Matrix t -> Matrix t1 -> Matrix t2)
    -> MatrixBlock t
    -> MatrixBlock t1
    -> MatrixBlock t2
spew op (Dense x) (Dense y) = Dense (x `op` y)
spew op (Dense x) (Diagonal l) = Dense (x `op` blockDiag l)
spew op (Diagonal l) (Dense x) = Dense (blockDiag l `op` x)
spew op (Diagonal x) (Diagonal y) = Diagonal (zipWith op x y) -- allow different sizes?
spew _ _ _ = impossible "spew in Sparse"

spMul :: (Product t)
      => MatrixBlock t -> MatrixBlock t -> MatrixBlock t
spMul Zero _ = Zero
spMul _ Zero = Zero
spMul (Dense x) (Dense y) = Dense (x<>y)
spMul x@(Dense _) y@(Diagonal _) = spTrans (spMul (spTrans y) (spTrans x))
spMul (Diagonal l) (Dense x) = Dense (blockDiagMul l x)
spMul (Diagonal x) (Diagonal y) = Diagonal (zipWith (<>) x y) -- allow different sizes?

-------------------------------

bTrans :: SparseMatrix t -> SparseMatrix t
bTrans (SparseMatrix r c at) = SparseMatrix c r at'
    where at' (i,j) = spTrans (at (j,i))

bMul :: (Product t, Container Vector t, Num (Vector t))
     => SparseMatrix t -> SparseMatrix t -> SparseMatrix t
bMul (SparseMatrix rx cx atx) (SparseMatrix _ry cy aty) = mkSparse l where
    l = [((i,j), foldl' spAdd Zero [atx (i,k) `spMul` aty (k,j) | k<-ks])| i<-is, j<-js]
    ks = [0..cx-1]
    is = [0..rx-1]
    js = [0..cy-1]

--------------------------------------------

blockDiag :: (Container Vector t, Num (Vector t)) 
          => [Matrix t] -> Matrix t
blockDiag bs = fromBlocks [ [ f i j | i<-ns ] | j<-ns ] where
    ns = [ 0 .. length bs -1]
    f i j | i==j = bs!!i
          | otherwise = 0

--blockDiagSolve :: (Field t) => [Matrix t] -> Matrix t -> Matrix t
--blockDiagSolve bs m = blockDiagOp linearSolve bs m

blockDiagSolveLU :: (Field t) => [(Matrix t, [Int])] -> Matrix t -> Matrix t
blockDiagSolveLU lus m = blockDiagOpG (rows.fst) luSolve lus m

blockDiagMul :: (Product t) => [Matrix t] -> Matrix t -> Matrix t
blockDiagMul bs m = blockDiagOp (<>) bs m

blockDiagOp :: (Element t2, Element t1) 
            => (Matrix t -> Matrix t1 -> Matrix t2)
            -> [Matrix t]
            -> Matrix t1
            -> Matrix t2
blockDiagOp op bs m = blockDiagOpG rows op bs m

blockDiagOpG :: (Element t1, Element t) 
             => (a -> Int) -> (a -> Matrix t -> Matrix t1)
             -> [a]
             -> Matrix t
             -> Matrix t1
blockDiagOpG fbs op bs m = fromBlocks (map return xs) where
    rs = map fbs bs
    ms = map head $ toBlocks rs [cols m] m
    xs = zipWith op bs ms

