module Vision.Bundle(
    sGError, sEError, sEEError, sEAError, objectQualityS, poseQualityS,
    geaG, recompPts, recompCamsSel,
    mySparseBundle, onlyPoints, onlyCams, alterPointsCams,
    sbaG,
    module Vision.SparseRep) where

import Numeric.LinearAlgebra as LA
import qualified Data.Array as A
import Numeric.LinearAlgebra.Tensor
import Numeric.LinearAlgebra.Array.Solve
import Numeric.LinearAlgebra.Array.Util
import Data.List
import Vision.Camera
import Vision.Geometry
import Vision.Stereo
import Vision.Estimation
import Util.Quaternion
import Vision.TensorRep
import Vision.Multiview
import Vision.SparseRep
import Util.Sparse
import Data.Function(on)
import Data.Maybe(fromJust)
import Util.Misc(splitEvery,mean,debug,norm,arrayOf,replaceAt,myintersect)

aug lambda m = m + diag (constant lambda (rows m))

-- | geometric reprojection error of a sparse visual problem
sGError :: SparseVP -> (Double,Double)
sGError s = (2*e2, sqrt e2) where
    obs = lObs s
    n = fromIntegral (length obs)
    c = arrayOf (zipWith (<>) (sKal s) (sCam s))
    p = arrayOf (sPts s)
    g s ((i,j), [x,y]) = s + (x-a/w)^ 2 + (y-b/w)^2
        where [a,b,w] = toList $ c j <> p i
    e2 = foldl' g 0 obs / (2*n)

-- | geometric epipolar error of a sparse visual problem
sEError :: SparseVP -> (Double,Double)
sEError s = (2*e2, sqrt e2) where
    obs = lObs s
    n = fromIntegral (length obs)
    c = arrayOf (zipWith (<>) (sKal s) (sCam s))
    cen = arrayOf (map (nullVector) (sCam s))
    p = arrayOf (sPts s)
    e2 = foldl' g 0 obs / (2*n)

    lEpi v j w = toList $ unitary $ (c v <> cen w) `cross` (c v <> p j)

    g ac ((j,v), x) = ac + mean [ distE2 x (lEpi v j w) | w <- [0..snC s-1] \\ [v]]


distE2 [x,y] [l1,l2,l3] = (l1*x + l2*y + l3)^2/(l1^2 + l2^2)

-- | geometric empiric epipolar error of a sparse visual problem
sEEError :: SparseVP -> (Double,Double)
sEEError s = (2*e2, sqrt e2) where
    obs = lObs s
    n = fromIntegral (length obs)
    c = arrayOf (zipWith (<>) (sKal s) (sCam s))
    cen = arrayOf (map (nullVector) (sCam s))
    ic = arrayOf (map (pinv) (sCam s))
    p = arrayOf (sPts s)
    e2 = foldl' g 0 obs / (2*n)

    lEpi v j w = toList $ unitary $ fundamentalFromCameras (c w) (c v) <> (fromList (aObs s A.!  (j,w)))

    lEpi'' v j w = toList $ unitary $ fundamentalFromCameras (c w) (c v) <> (c w <> p j)

    g ac ((j,v), x) = ac + mean [ distE2 x (lEpi v j w) | w <- v_of_p s j \\ [v]]


-- replicate value from pairs, as used in GEA
-- algebraic epipolar error
sEAError :: SparseVP -> (Double,Double)
sEAError s = (2*e2, sqrt e2) where
    obs = lObs s
    n = fromIntegral (length obs)
    ik = arrayOf (map inv $ sKal s)
    c = arrayOf (zipWith (<>) (sKal s) (sCam s))
    cen = arrayOf (map (nullVector) (sCam s))
    ic = arrayOf (map (pinv) (sCam s))
    p = arrayOf (sPts s)
    e2 = foldl' g 0 obs / (2*n)

    lEpi v j w = toList $ unitary $ ik w <> (fundamentalFromCameras (c w) (c v) <> (fromList (aObs s A.!  (j,w))))

    g ac ((j,v), x) = ac + mean [ distAlgE2 x' (lEpi v j w) | w <- v_of_p s j \\ [v]]
        where [x'] = ht (ik v) [x]

distAlgE2 [x,y] [l1,l2,l3] = (l1*x + l2*y + l3)^2

---------------------------------------------------------------

objectQualityS :: SparseVP -> SparseVP -> Double
objectQualityS = metricConsis (map inHomog . sPts)

poseQualityS :: SparseVP -> SparseVP -> Double
poseQualityS = metricConsis (map (inHomog.nullVector) . sCam)

----------------------------------------------------------------

prepareEpipolar s = (getBlocks, vsol, newSol, fcost) where
    obs = debug "dim f " (sum.map (rows.snd)) $ debug "links " length $ epiObs s
    mbk0s = replicate (snC s) (Just (kgen 1))
    origins = zipWith cameraModelOrigin mbk0s (sCam s)
    newSol sol = recompPts $ s {sCam = zipWith ($) (map projectionAt'' origins) (takesV (replicate (snC s) 6) sol) }
    vsol = constant (0::Double) (6*snC s)
    preJaco vs = zipWith auxCamJac origins (takesV (replicate (snC s) 6) vs)

    getBlocks vs =  (f,j) where
        x = map g (zip [0.. ] obs)
        f = mkSparse $ map head x
        j = mkSparse $ concatMap tail x
        jaq = preJaco vs
        g (k, ((i,j), d)) = [((k,0), Dense (d <>f)),
                             ((k,i), Dense (d <>f1)),
                             ((k,j), Dense (d <>f2))
                            ]
            where (f,f1,f2) = epipolarMiniJac (jaq!!i) (jaq!!j)

    fcost sol = 1E4 * sqrt (norm v ^ 2 / fromIntegral (dim v))
        where v = flatten $ toDense $ fst $ getBlocks sol

-- triangulation requires normalized coordinates
recompPts s = s { sPts = newps } where
    c = arrayOf (sCam s)
    ako' = init . toList . ako s
    newps = map (fromList.(++[1]).f) [0.. snP s -1]
    f p = triangulate1 cs ps where
        (cs,ps) = unzip $ map g (v_of_p s p)
            where g k = (c k, ako' (p,k))

-- obtains imperfect rotations
recompCams s = s { sCam = newcs } where
    p = arrayOf (sPts s)
    ako' = init . toList . ako s
    newcs = map f [0.. snC s -1]
    f v = estimateCamera img world where
        (world,img) = unzip $ map g (p_of_v s v)
            where g k = ((init.toList) (p k), ako' (k,v))


ptsVisibleBy s sel = reverse $ map head . filter ((>1).length) $ group $ sort $ concatMap (p_of_v s) sel


recompPtsSel s sel = s { sPts = replaceAt vis newps (sPts s) } where
    c = arrayOf (sCam s)
    ako' = init . toList . ako s
    vis = ptsVisibleBy s sel
    newps = map (fromList.(++[1]).f) vis
    f p = triangulate1 cs ps where
        (cs,ps) = unzip $ map g (reverse (v_of_p s p) `myintersect` (reverse (sort sel)))
            where g k = (c k, ako' (p,k))

recompCamsSel s sel desi = s { sCam = replaceAt desi newcs (sCam s') } where
    s' = recompPtsSel s sel
    p = arrayOf (sPts s')
    ako' = init . toList . ako s'
    selpts = ptsVisibleBy s' sel
    newcs = map f desi
    f v = snd $ sepCam $ estimateCamera img world where
        (world,img) = unzip $ map g (p_of_v s' v `myintersect` selpts)
            where g k = ((init.toList) (p k), ako' (k,v))

--------------------------------------------------------------

myepi3 s k = newSol . fst . debug "epipolar errors: " (map round. snd) . 
                      -- snd .
            optimize 0 1 k
            update
            fcost
            $ vsol
    where update = naiveLevmarS 0.001 mods
          (mods,vsol,newSol,fcost) = prepareEpipolar s


naiveLevmarS lambda m sol = sol' where
    fun = fst (m sol)
    jac = snd (m sol)
    hes = toDense $ bMul (bTrans jac) jac
    grad = toDense $ bMul (bTrans jac) fun
    sol' = sol - (flatten $ linearSolve (aug lambda hes) grad)

-- at the current solution!! (just to test, it recomputes a lot of things...)
epiHessian s = hes where
    jac = snd (m vsol)
    hes = toDense $ bMul (bTrans jac) jac
    (m,vsol,_,_) = prepareEpipolar s

---------------------------------------------------------------

prepareEpipolarSelect used free s = (getBlocks, vsol, newSol, fcost) where
    info = "links = " ++ show (length obs)
    obs = epiObs s
    mbk0s = replicate (snC s) (Just (kgen 1))
    origins = zipWith cameraModelOrigin mbk0s (sCam s)
    newSol sol = s {sCam = zipWith ($) (map projectionAt'' origins) (fill sol) }
    vsol = constant (0::Double) (6* length free)

    fill minisol = map snd $ sortBy (compare `on` fst) $ zip free (takesV (replicate (length free) 6) minisol) ++ zip ([0..snC s -1] \\ free) (repeat (constant 0 6))

    preJaco vs = zipWith auxCamJac origins (fill vs)
    fromSel = fromJust . flip lookup (zip free [0..])

    getBlocks vs =  (f,j) where
        x = filter (not . null) $ map g (zip [0.. ] obs)
        f = mkSparse $ map head x
        j = mkSparse $ concatMap tail x
        jaq = preJaco vs

        g (k, ((i,j), d))
                | i `elem` free && j `elem` free = [a,b,c]
                | i `elem` free && j `elem` used = [a,b]
                | i `elem` used && j `elem` free = [a,c]
                | otherwise                      = []
            where (f,f1,f2) = epipolarMiniJac (jaq!!i) (jaq!!j)
                  a = ((k,0), Dense (d <>f))
                  b = ((k,fromSel i), Dense (d <>f1))
                  c = ((k,fromSel j), Dense (d <>f2))

    fcost sol = 1E4 * sqrt (norm v ^ 2 / fromIntegral (dim v))
        where v = flatten $ toDense $ fst $ getBlocks sol

geaG delta maxIt lambda used free s = (newSol sol, info) where
    (sol, errs) = optimize 0 delta maxIt update fcost vsol
    update = naiveLevmarS lambda mods
    (mods,vsol,newSol,fcost) = prepareEpipolarSelect used free s
    info = "epipolar errors: " ++ show (map round errs)

---------------------------------------------------------------

mySparseBundleLMG met l0 df uf s k = newSol $ fst $ debug "bundle errors: " (map (round.(1E2*)) . snd) $
            optimizeLM 0 1 k
            update
            fcost
            vsol
            l0 df uf
    where (mods,vsol,newSol,fcost) = prepareBundle s
          update l s = s - (met l . mods) s

------------------------------------------------------------------

mySparseBundle = mySparseBundleLMG sparseSolve 1 (/10) (*10)
--mySparseBundle lambda = mySparseBundleG (sparseSolve lambda)

onlyPoints lambda s = mySparseBundleG (solvePoints lambda (snC s)) s

onlyCams lambda s = mySparseBundleG (solveCams lambda (snP s)) s

alterPointsCams k = (!!k) . iterate f
    where f s = onlyCams 0.001 (onlyPoints 0.001 s 1) 1


mySparseBundleG met s k = newSol . fst . debug "bundle errors: " (map (round.(1E2*)) . snd) . 
                      -- snd .
            optimize 0 1 k
            update
            fcost
            $ vsol
    where (mods,vsol,newSol,fcost) = prepareBundle s
          update s = s - (met . mods) s

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
        where g v = sqrt (norm v ^ 2  / fromIntegral (dim v))





sparseSolve lambda (h_cc',h_pp',h_cp,g_c,g_p) = join (map flatten [solc,solp]) where
    h_cc =  map (aug lambda) h_cc'
    lu_h_pp =  map (luPacked.aug lambda) h_pp'
    [f,g] = map ( (h_cp <>) .  blockDiagSolveLU lu_h_pp) [trans h_cp, asColumn g_p]
    sysc =  blockDiag h_cc - f
    rhc  = asColumn g_c - g
    solc = linearSolve sysc rhc
    solp = blockDiagSolveLU lu_h_pp (asColumn g_p - trans h_cp <> solc)

solvePoints lambda nc (_,h_pp',_,_,g_p) = join [solc,flatten solp] where
    lu_h_pp =  map (luPacked.aug lambda) h_pp'
    solc = constant 0 (6*nc)
    solp = blockDiagSolveLU lu_h_pp (asColumn g_p)

solveCams lambda np (h_cc',_,_,g_c,_) = join [flatten solc, solp] where
    lu_h_cc =  map (luPacked.aug lambda) h_cc'
    solp = constant 0 (3*np)
    solc = blockDiagSolveLU lu_h_cc (asColumn g_c)

-- at the current solution!! (just to test, it recomputes a lot of things...)
hessianCamsBundle lambda s = sysc where
    (m,vsol,_,_) = prepareBundle s
    (h_cc',h_pp',h_cp,_,_) = m vsol
    h_cc =  map (aug lambda) h_cc'
    lu_h_pp =  map (luPacked.aug lambda) h_pp'
    f = h_cp <> blockDiagSolveLU lu_h_pp (trans h_cp)
    sysc =  blockDiag h_cc - f

----------------------------------------------------------------
----------------------------------------------------------------

sbaG = mySparseBundleLMSelectG sparseSolve 1 (/10) (*10)

mySparseBundleLMSelectG met l0 df uf used free s k = newSol $ fst $ debug "bundle errors: " (map (round.(1E2*)) . snd) $
            optimizeLM 0 1 k
            update
            fcost
            vsol
            l0 df uf
    where (mods,vsol,newSol,fcost) = prepareBundleSelect used free s
          update l s = s - (met l . mods) s

---------------------------------------------------------

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
        where g v = sqrt (norm v ^ 2  / fromIntegral (dim v))

