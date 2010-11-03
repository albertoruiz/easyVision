{-# LANGUAGE FlexibleContexts #-}

module Vision.Gea(
    geaFull,
    gea,
    geaG,
    recompPts, recompPtsSel, recompCams, recompCamsSel,
    epiHessian,
    module Vision.SparseRep) where

import Numeric.LinearAlgebra as LA hiding (i)
import Data.List
import Vision.Camera
import Vision.Stereo(triangulate1)
import Util.Optimize(optimize)
import Vision.SparseRep
import Util.Sparse
import Data.Function(on)
import Data.Maybe(fromJust)
import Util.Misc(debug,norm,arrayOf,replaceAt,myintersect,round',sqr,Mat,Vec)


-- | global epipolar adjustment using all views (default parameters)
geaFull :: SparseVP -> SparseVP
geaFull s = recompPts $ gea sel sel s where sel = [0.. snC s -1]


-- | global epipolar adjustment for given used and free views (without point reconstruction)
gea :: [Int] -> [Int] -> SparseVP -> SparseVP
gea used free = fst . debug "" snd . geaG 1 10 0.001 used free

-- | ojo fcost
geaG :: Double -- ^ delta
     -> Int    -- ^ maxit
     -> Double -- ^ lambda
     -> [Int]  -- ^ used
     -> [Int]  -- ^ free
     -> SparseVP -- ^ problem
     -> (SparseVP, String) -- ^ solution and info
geaG delta maxIt lambda used free s = (newSol sol, info) where
    (sol, errs) = optimize 0 delta maxIt update fcost vsol
    update = newtonStep lambda mods
    (mods,vsol,newSol,_fcost) = prepareEpipolarSelect used free s
    info = "epipolar errors: " ++ show (map round' errs)
    fcost solu = 1E2 * (snd $ sGError (recompPts $ newSol solu))

-- | one optimization step (constant lambda)
newtonStep :: Double -> (Vec -> (SMat, SMat)) -> Vec -> Vec
newtonStep lambda m sol = sol' where
    fun = fst (m sol)
    jac = snd (m sol)
    hes = toDense $ bMul (bTrans jac) jac
    grad = toDense $ bMul (bTrans jac) fun
    sol' = sol - (flatten $ linearSolve (aug lambda hes) grad)
    aug lam mat = mat + diag (constant lam (rows mat))

prepareEpipolarSelect
    :: [Int]
    -> [Int]
    -> SparseVP
    -> (Vec -> (SMat, SMat), Vec, Vec -> SparseVP, Vec -> Double)
prepareEpipolarSelect used free s = (getBlocks, vsol, newSol, fcost) where
    --info = "links = " ++ show (length obs)
    obs = epiObs s
    mbk0s = replicate (snC s) (Just (kgen 1))
    origins = zipWith cameraModelOrigin mbk0s (sCam s)
    newSol sol = s {sCam = zipWith ($) (map projectionAt'' origins) (fill sol) }
    vsol = constant (0::Double) (6* length free)

    fill minisol = map snd $ sortBy (compare `on` fst) $ zip free (takesV (replicate (length free) 6) minisol) ++ zip ([0..snC s -1] \\ free) (repeat (constant 0 6))

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

    fcost sol = 1E4 * sqrt (sqr (norm v) / fromIntegral (dim v))
        where v = flatten $ toDense $ fst $ getBlocks sol


---------------------------------------------------------------

-- | at the current solution!! (just to test, it recomputes a lot of things...)
epiHessian :: SparseVP -> Mat
epiHessian s = hes where
    jac = snd (m vsol)
    hes = toDense $ bMul (bTrans jac) jac
    (m,vsol,_,_) = prepareEpipolarSelect sel sel s where sel = [0.. snC s -1]

---------------------------------------------------------------

-- | triangulation of 3d points (using normalized coordinates)
recompPts :: SparseVP -> SparseVP
recompPts s = s { sPts = newps } where
    c = arrayOf (sCam s)
    ako' = init . toList . ako s
    newps = map (fromList.(++[1]).f)  [0.. snP s -1]
    f p = triangulate1 cs ps where
        (cs,ps) = unzip $ map g (v_of_p s p)
            where g k = (c k, ako' (p,k))

-- | obtains imperfect rotations
recompCams :: SparseVP -> SparseVP
recompCams s = s { sCam = newcs } where
    p = arrayOf (sPts s)
    ako' = init . toList . ako s
    newcs = map f [0.. snC s -1]
    f v = estimateCameraRaw img world where
        (world,img) = unzip $ map g (p_of_v s v)
            where g k = ((init.toList) (p k), ako' (k,v))

-- | reconstruct selected points
recompPtsSel :: SparseVP -> [Int] -> SparseVP
recompPtsSel s sel = s { sPts = replaceAt vis newps (sPts s) } where
    c = arrayOf (sCam s)
    ako' = init . toList . ako s
    vis = ptsVisibleBy s sel
    newps = map (fromList.(++[1]).f) vis
    f p = triangulate1 cs ps where
        (cs,ps) = unzip $ map g (reverse (v_of_p s p) `myintersect` (reverse (sort sel)))
            where g k = (c k, ako' (p,k))

-- | reconstruct selected views
recompCamsSel :: SparseVP -> [Int] -> [Int] -> SparseVP
recompCamsSel s sel desi = s { sCam = replaceAt desi newcs (sCam s') } where
    s' = recompPtsSel s sel
    p = arrayOf (sPts s')
    ako' = init . toList . ako s'
    selpts = ptsVisibleBy s' sel
    newcs = map f desi
    f v = snd $ sepCam $ estimateCamera img world where
        (world,img) = unzip $ map g (p_of_v s' v `myintersect` selpts)
            where g k = ((init.toList) (p k), ako' (k,v))
