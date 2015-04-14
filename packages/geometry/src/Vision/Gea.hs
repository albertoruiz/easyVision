{-# LANGUAGE FlexibleContexts #-}

module Vision.Gea(
    geaFull,
    gea,
    geaG,
    epiHessian
) where

import Numeric.LinearAlgebra.HMatrix as LA hiding (toDense, mkSparse)
import Data.List
import Vision.Camera
import Util.Optimize(optimize)
import Vision.Epipolar
import Vision.Types(Motion)
import Util.Sparse
import Data.Function(on)
import Data.Maybe(fromJust)
import Util.Misc(Mat,Vec)
import Util.Debug(debug)


-- | refine cameras using global epipolar adjustment (all views and default parameters)
geaFull :: EpiPairs -> Motion -> Motion
geaFull s cams = gea sel sel s cams where sel = [0.. length cams -1]


-- | global epipolar adjustment for given used and free views
gea :: [Int] -> [Int] -> EpiPairs -> Motion -> Motion
gea used free initcams = fst . debug "" snd . geaG 1 10 0.001 used free initcams


geaG :: Double -- ^ delta
     -> Int    -- ^ maxit
     -> Double -- ^ lambda
     -> [Int]  -- ^ used
     -> [Int]  -- ^ free
     -> EpiPairs    -- ^ problem
     -> Motion  -- ^ initial solution
     -> (Motion, String) -- ^ solution and info
geaG delta maxIt lambda used free s initcams = (newSol sol, info) where
    (sol, errs) = optimize 0 delta maxIt update fcost vsol
    update = newtonStep lambda mods
    (mods,vsol,newSol,fcost) = prepareEpipolarSelect used free s initcams
    info = "epipolar errors: " ++ show (map round errs)

-- | one optimization step (constant lambda)
newtonStep :: Double -> (Vec -> (SMat, SMat)) -> Vec -> Vec
newtonStep lambda m sol = sol' where
    fun = fst (m sol)
    jac = snd (m sol)
    hes = toDense $ bMul (bTrans jac) jac
    grad = toDense $ bMul (bTrans jac) fun
    sol' = sol - (flatten $ linearSolveLS (aug lambda hes) grad)
    aug lam mat = mat + diag (konst lam (rows mat))

prepareEpipolarSelect
    :: [Int]
    -> [Int]
    -> EpiPairs
    -> Motion
    -> (Vec -> (SMat, SMat), Vec, Vec -> Motion, Vec -> Double)
prepareEpipolarSelect used free obs initcams = (getBlocks, vsol, newSol, fcost) where
    --info = "links = " ++ show (length obs)
    nC = length initcams
    mbk0s = replicate nC (Just (kgen 1))
    origins = zipWith cameraModelOrigin mbk0s initcams
    newSol sol = zipWith ($) (map projectionAt'' origins) (fill sol)
    vsol = konst (0::Double) (6* length free)

    fill minisol = map snd $ sortBy (compare `on` fst) $ zip free (takesV (replicate (length free) 6) minisol) ++ zip ([0..nC -1] \\ free) (repeat (konst 0 6))

    preJaco vs = zipWith auxCamJac origins (fill vs)
    fromSel = fromJust . flip lookup (zip free [0..])

    getBlocks vs =  (ff,jj) where
        x = filter (not . null) $ map g (zip [0.. ] obs)
        ff = mkSparse $ map head x
        jj = mkSparse $ concatMap tail x
        jaq = preJaco vs

        g (k, ((i,j), epi))
                | i `elem` free && j `elem` free = [a,b,c]
                | i `elem` free && j `elem` used = [a,b]
                | i `elem` used && j `elem` free = [a,c]
                | otherwise                      = []
            where (f,f1,f2) = epipolarMiniJac (jaq!!i) (jaq!!j)
                  a = ((k,0), Dense (d <>f))
                  b = ((k,fromSel i), Dense (d <>f1))
                  c = ((k,fromSel j), Dense (d <>f2))
                  d = m_hat epi

    fcost sol = 1E4 * sqrt ((norm_2 v)**2 / fromIntegral (size v))
        where v = flatten $ toDense $ fst $ getBlocks sol


---------------------------------------------------------------

epiHessian :: EpiPairs -> Motion -> Mat
epiHessian s cams = hes where
    jac = snd (m vsol)
    hes = toDense $ bMul (bTrans jac) jac
    (m,vsol,_,_) = prepareEpipolarSelect sel sel s cams where sel = [0.. length cams -1]

---------------------------------------------------------------

