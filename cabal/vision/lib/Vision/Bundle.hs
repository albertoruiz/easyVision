{-# LANGUAGE FlexibleContexts #-}

module Vision.Bundle(
    mySparseBundle, onlyPoints, onlyCams, alterPointsCams,
    hessianCamsBundle,
    sbaG,
    module Vision.SparseRep) where

import Numeric.LinearAlgebra as LA hiding (i)
import qualified Data.Array as A

import Vision.Camera
import Vision.Geometry

import Util.Optimize(optimize,optimizeLM)

import Vision.SparseRep
import Util.Sparse

import Util.Misc(debug,norm,arrayOf,round',Vec,Mat,sqr)

aug :: Double -> Mat -> Mat
aug lambda m = m + diag (constant lambda (rows m))

mySparseBundleLMG
    :: (Double -> ([Mat],[Mat], Mat, Vec, Vec)
    -> Vec)
    -> Double
    -> (Double -> Double)
    -> (Double -> Double)
    -> SparseVP
    -> Int
    -> SparseVP
mySparseBundleLMG met l0 df uf s k = newSol $ fst $ debug "bundle errors: " (map (round'.(1E2*)) . snd) $
            optimizeLM 0 1 k
            update
            fcost
            vsol
            l0 df uf
    where (mods,vsol,newSol,fcost) = prepareBundle s
          update l sol = sol - (met l . mods) sol

------------------------------------------------------------------

mySparseBundle :: SparseVP -> Int -> SparseVP
mySparseBundle = mySparseBundleLMG sparseSolve 1 (/10) (*10)

onlyPoints :: Double -> SparseVP -> Int -> SparseVP
onlyPoints lambda s = mySparseBundleG (solvePoints lambda (snC s)) s

onlyCams :: Double -> SparseVP -> Int -> SparseVP
onlyCams lambda s = mySparseBundleG (solveCams lambda (snP s)) s

alterPointsCams :: SparseVP -> SparseVP
alterPointsCams = fst . debug "BundelAlter: " (map (round'.(1E2*)) . snd) .
            optimize 0 1 20
            (\s -> onlyPoints 0.001 (onlyCams 0.001 s 1) 1)
            (snd.sGError)


mySparseBundleG
    :: (([Mat],[Mat],Mat,Vec,Vec) -> Vec)
    -> SparseVP
    -> Int
    -> SparseVP
mySparseBundleG met s k = newSol . fst . debug "bundle errors: " (map (round'.(1E2*)) . snd) .
                      -- snd .
            optimize 0 1 k
            update
            fcost
            $ vsol
    where (mods,vsol,newSol,fcost) = prepareBundle s
          update sol = sol - (met . mods) sol

prepareBundle
    :: SparseVP
    -> (Vec -> ([Mat], [Mat], Mat, Vec, Vec), Vec, Vec -> SparseVP, Vec -> Double)
prepareBundle s = (mods, vsol, newSol, fcost) where
    mbk0s = map Just (sKal s)
    origins = zipWith cameraModelOrigin mbk0s (sCam s)
    newSol sol = s { sCam = newcams, sPts = newpts }
        where solc = takesV (replicate (snC s) 6) sol
              newcams = zipWith ($) (map projectionAt'' origins) solc
              solp = takesV (replicate (snP s) 3) (subVector (snC s *6) (snP s *3) sol)
              newpts = map homog solp

    vsol = join (constant (0::Double) (6*snC s) : (map inHomog (sPts s)))

    preJaco vs = zipWith auxCamJacK origins (takesV (replicate (snC s) 6) vs)

    getBlocks vs = (lf,ljc,ljp) where
        jaq = arrayOf (preJaco vs)
        pt = arrayOf $ takesV (replicate (snP s) 3) (subVector (snC s *6) (snP s *3) vs)

        (lf,ljc,ljp) = unzip3 (map g (lObs s))
        g (a@(p,v),x) = ((a, f - fromList x),(a,jc),(a,jp))
            where (f,jc,jp) = projectionDerivAt' (jaq v) (pt p)

    genArrays (lf,ljc,ljp) = (bf, bc, bp) where
        dms = ((0,0),(snP s-1,snC s-1))
        bf = curry (A.accumArray (flip const) 0 dms lf   A.!)
        bc = curry (A.accumArray (flip const) ((2><6)[0..]) dms ljc  A.!)
        bp = curry (A.accumArray (flip const) ((2><3)[0..]) dms ljp  A.!)

    getSpSys (bf, bc, bp) = (h_cc,h_pp,h_cp,g_c,g_p) where
        vs = [0 .. snC s -1]
        vOf = v_of_p s
        ps = [0 .. snP s -1]
        pOf = p_of_v s

        h_cp = fromBlocks $ [[trans (bc p v) <> (bp p v) | p <- ps ] | v <- vs ]
        h_cc = [ sum [ trans (bc p v) <> (bc p v) | p <- pOf v] | v <- vs]
        h_pp = [ sum [ trans (bp p v) <> (bp p v) | v <- vOf p] | p <- ps]
        g_c = join [ sum [ (bf p v) <> (bc p v) | p <- pOf v] | v <- vs]
        g_p = join [ sum [ (bf p v) <> (bp p v) | v <- vOf p] | p <- ps]

    mods = getSpSys . genArrays . getBlocks

    fcost = g . join . map snd . (\(x,_,_)->x) . getBlocks
        where g v = sqrt ( sqr(norm v) / fromIntegral (dim v))



sparseSolve :: Double -> ([Mat], [Mat], Mat, Vec, Vec) -> Vec
sparseSolve lambda (h_cc',h_pp',h_cp,g_c,g_p) = join (map flatten [solc,solp]) where
    h_cc =  map (aug lambda) h_cc'
    lu_h_pp =  map (luPacked.aug lambda) h_pp'
    [f,g] = map ( (h_cp <>) .  blockDiagSolveLU lu_h_pp) [trans h_cp, asColumn g_p]
    sysc =  blockDiag h_cc - f
    rhc  = asColumn g_c - g
    solc = linearSolve sysc rhc
    solp = blockDiagSolveLU lu_h_pp (asColumn g_p - trans h_cp <> solc)


solvePoints :: Double -> Int -> (t, [Mat], t1, t2, Vec) -> Vec
solvePoints lambda nc (_,h_pp',_,_,g_p) = join [solc,flatten solp] where
    lu_h_pp =  map (luPacked.aug lambda) h_pp'
    solc = constant 0 (6*nc)
    solp = blockDiagSolveLU lu_h_pp (asColumn g_p)

solveCams :: Double -> Int -> ([Mat], t, t1, Vec, t2) -> Vec
solveCams lambda np (h_cc',_,_,g_c,_) = join [flatten solc, solp] where
    lu_h_cc =  map (luPacked.aug lambda) h_cc'
    solp = constant 0 (3*np)
    solc = blockDiagSolveLU lu_h_cc (asColumn g_c)

-- at the current solution!! (just to test, it recomputes a lot of things...)
hessianCamsBundle :: Double -> SparseVP -> Mat
hessianCamsBundle lambda s = sysc where
    (m,vsol,_,_) = prepareBundle s
    (h_cc',h_pp',h_cp,_,_) = m vsol
    h_cc =  map (aug lambda) h_cc'
    lu_h_pp =  map (luPacked.aug lambda) h_pp'
    f = h_cp <> blockDiagSolveLU lu_h_pp (trans h_cp)
    sysc =  blockDiag h_cc - f

----------------------------------------------------------------

sbaG :: t -> t1 -> SparseVP -> Int -> SparseVP
sbaG = mySparseBundleLMSelectG sparseSolve 1 (/10) (*10)

mySparseBundleLMSelectG
    :: (t2
        -> ([Mat],
            [Mat],
            Mat,
            Vec,
            Vec)
        -> Vec)
    -> t2
    -> (t2 -> t2)
    -> (t2 -> t2)
    -> t
    -> t1
    -> SparseVP
    -> Int
    -> SparseVP
mySparseBundleLMSelectG met l0 df uf used free s k = newSol $ fst $ debug "bundle errors: " (map (round'.(1E2*)) . snd) $
            optimizeLM 0 1 k
            update
            fcost
            vsol
            l0 df uf
    where (mods,vsol,newSol,fcost) = prepareBundleSelect used free s
          update l x = x - (met l . mods) x

---------------------------------------------------------

prepareBundleSelect
    :: t
    -> t1
    -> SparseVP
    -> (Vec
        -> ([Mat],
            [Mat],
            Mat,
            Vec,
            Vec),
        Vec,
        Vec -> SparseVP,
        Vec -> Double)
prepareBundleSelect used free s = (mods, vsol, newSol, fcost) where
    mbk0s = map Just (sKal s)
    origins = zipWith cameraModelOrigin mbk0s (sCam s)

-------

    newSol sol = s { sCam = newcams, sPts = newpts }
        where solc = takesV (replicate (snC s) 6) sol
              newcams = zipWith ($) (map projectionAt'' origins) solc
              solp = takesV (replicate (snP s) 3) (subVector (snC s *6) (snP s *3) sol)
              newpts = map homog solp

    vsol = join (constant (0::Double) (6*snC s) : (map inHomog (sPts s)))

    preJaco vs = zipWith auxCamJacK origins (takesV (replicate (snC s) 6) vs)

------

--     newSol sol = s {sCam = zipWith ($) (map projectionAt'' origins) (fill sol) }
--     vsol = constant (0::Double) (6* length free)
-- 
--     fill minisol = map snd $ sortBy (compare `on` fst) $ zip free (takesV (replicate (length free) 6) minisol) ++ zip ([0..snC s -1] \\ free) (repeat (constant 0 6))
-- 
--     preJaco vs = zipWith auxCamJac origins (fill vs)
--     fromSel = fromJust . flip lookup (zip free [0..])

-------


    getBlocks vs = (lf,ljc,ljp) where
        jaq = arrayOf (preJaco vs)
        pt = arrayOf $ takesV (replicate (snP s) 3) (subVector (snC s *6) (snP s *3) vs)

        (lf,ljc,ljp) = unzip3 (map g (lObs s))
        g (a@(p,v),x) = ((a, f - fromList x),(a,jc),(a,jp))
            where (f,jc,jp) = projectionDerivAt' (jaq v) (pt p)

    genArrays (lf,ljc,ljp) = (bf, bc, bp) where
        dms = ((0,0),(snP s-1,snC s-1))
        bf = curry (A.accumArray (flip const) 0 dms lf   A.!)
        bc = curry (A.accumArray (flip const) ((2><6)[0..]) dms ljc  A.!)
        bp = curry (A.accumArray (flip const) ((2><3)[0..]) dms ljp  A.!)

    getSpSys (bf, bc, bp) = (h_cc,h_pp,h_cp,g_c,g_p) where
        vs = [0 .. snC s -1]
        vOf = v_of_p s
        ps = [0 .. snP s -1]
        pOf = p_of_v s

        h_cp = fromBlocks $ [[trans (bc p v) <> (bp p v) | p <- ps ] | v <- vs ]
        h_cc = [ sum [ trans (bc p v) <> (bc p v) | p <- pOf v] | v <- vs]
        h_pp = [ sum [ trans (bp p v) <> (bp p v) | v <- vOf p] | p <- ps]
        g_c = join [ sum [ (bf p v) <> (bc p v) | p <- pOf v] | v <- vs]
        g_p = join [ sum [ (bf p v) <> (bp p v) | v <- vOf p] | p <- ps]

    mods = getSpSys . genArrays . getBlocks

    fcost = g . join . map snd . (\(x,_,_)->x) . getBlocks
        where g v = sqrt (norm v ^ (2::Int)  / fromIntegral (dim v))
