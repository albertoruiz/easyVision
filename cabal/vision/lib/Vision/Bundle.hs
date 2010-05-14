module Vision.Bundle where

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
import Vision.Multiview
import Util.Sparse
import Data.Function(on)
import Data.Maybe(fromJust)
import Util.Misc(splitEvery,mean,debug)

aug lambda m = m + diag (constant lambda (rows m))

-- A sparse visual problem
data SparseVP = SVP { v_of_p :: Int -> [Int] -- visible views of each point
                    , p_of_v :: Int -> [Int] -- points visible in each view
                    , lObs   :: [((Int,Int), [Double])]      -- raw sparse observations
                    , aObs   :: A.Array (Int,Int) ([Double]) -- fast access to raw with missing = 0,0,0
                    , snC :: Int -- number of views
                    , snP :: Int -- number of points
                    , sCam :: [Matrix Double] -- cameras (without K)
                    , sKal :: [Matrix Double] -- calibration matrices
                    , sPts :: [Vector Double] -- homogeneous 3D points
                    -- dep of sKal:
                    , epiObs :: [((Int,Int), Matrix Double)] -- reduced measurement matrix for each pair of views
                    , ako :: (Int, Int) -> Vector Double -- fast access to calibrated observations
                    }

-----------------------------------------------------------------------------------

loadRawTracks filepts = do
    s <- (filter (('#'/=).head.head) . map words . lines) `fmap` readFile filepts
    let pts = map ((fromList . (++[1])) . map read . take 3) s :: [Vector Double]
        g n [v,x,y] = ((n ::Int,read v :: Int), [read x ,read y :: Double])
        obsp = zipWith (map . g) [0..] $ map (splitEvery 3 . drop 4) s
    return (pts, obsp)

goSparse obsp = SVP { v_of_p = v_p,
                      p_of_v = (p_v A.!),
                      lObs = obs,
                      aObs = ao,
                      snP = nP,
                      snC = nC,
                      sPts = undefined,
                      sCam = undefined,
                      sKal = undefined,
                      ako = undefined,
                      epiObs = undefined
                      } where
    v_p = arrayOf $ map (map (snd.fst)) obsp
    obs = concat obsp
    nC = maximum (map (snd.fst) obs) + 1
    nP = maximum (map (fst.fst) obs) + 1
    p_v = A.accumArray (flip (:)) [] (0,nC-1) (map (swap.fst) obs) where swap (a,b) = (b,a)
    ao = A.accumArray f ([0,0,0]) ((0,0),(nP-1,nC-1)) obs where f _ [x,y] = [x,y,1]


-- | updates sKal, ako and epiObs
recalibrate :: [Matrix Double] -> SparseVP -> SparseVP
recalibrate ks s = s' {epiObs = mkEpiObs s'} where
    s' = s { sKal = ks, ako = ako'} 
    ik = arrayOf (map inv ks)
    ako' = (A.accumArray f (fromList [0,0,0]) ((0,0),(snP s -1,snC s-1)) obs' A.!)
        where f _ ((p_,v),[x,y]) = ik v <> fromList [x,y,1]
              obs' = map (\(a,b)-> (a,(a,b))) (lObs s)


-- | Creates a sparse visual problem, from raw data in a file with SBA format
loadTracks :: FilePath -> IO SparseVP
loadTracks filepts = do
    (pts,obsp) <- loadRawTracks filepts
    return (goSparse obsp) { sPts = pts }

-- | complete a sparse visual problem with cameras and calibration info from files with SBA format
loadSVP :: FilePath -> FilePath -> FilePath -> IO SparseVP
loadSVP filepoints filecams filecalib = do
    s <- loadTracks filepoints
    cams <- loadQCams filecams
    k <- loadMatrix filecalib
    return $ recalibrate (replicate (snC s) k) s { sCam = cams }


loadLinks :: FilePath -> IO [(Int,Int)]
loadLinks filelinks = (map (l2p.words) .lines) `fmap` readFile filelinks
    where l2p [a,b] = (read a , read b)

loadTracks' :: FilePath -> IO SparseVP
loadTracks' filename = do
    let g p = map (\(v,x)->((p,v),x))
    obsp <- (zipWith g [0..] . map (filter ((>0).head.snd) . zip [0..] . splitEvery 2 . map read. words) . lines) `fmap` readFile filename 
    return (goSparse obsp)


loadQCams filecams = (map (camFromQuat . map read . words) . lines) `fmap` readFile filecams
  where
    camFromQuat [s, a, b, c, x, y, z] = cam
        where q = Quat s (fromList [a,b,c])
              r = getRotation q
              cen = fromList [x,y,z]
              cam = fromBlocks [[r, asColumn cen]]

-------------------------------------------------------------


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

-- | Tensor version of a sparse problem.
-- (Since it will probably be not dense, the geometric error cannot be naively computed.)
-- | The views are calibrated, and the cameras have K=id.
tensorProblem :: [Int] -> [Int] ->  SparseVP -> VProb
tensorProblem selpts selcams s = prob where
    cs = zipWith (<>) (sKal s) (sCam s)

    sView = (subindex "n" . map (subindex "c" . map vector) . splitEvery (length selcams) $ selected) !> "1v"

    selected = [aObs s A.! (p,v) | p <- selpts, v <- selcams]

    prob = VProb { p2d = sView,
                   cam = (subindex "c" $ map (fromMatrix Contra Co) $ selectPos selcams $ cs) !> "1v 2x",
                   p3d = (subindex "n" $ map (fromVector Contra) $ selectPos selpts $ sPts s) !> "1x",
                   l2d = undefined,
                   l3d = undefined  }

-- | Tensor version of a sparse problem.
-- (Since it will probably be not dense, the geometric error cannot be naively computed.)
-- | The views are calibrated, and the cameras have K=id.
tensorProblemK :: [Int] -> [Int] -> SparseVP -> VProb
tensorProblemK selpts selcams s = prob where
    ikt = fromMatrix Contra Co (inv (head (sKal s))) !"wv"  -- BAD!! (use all)

    sView = (subindex "n" . map (subindex "c" . map vector) . splitEvery (length selcams) $ selected) !> "1v"

    selected = [aObs s A.! (p,v) | p <- selpts, v <- selcams]

    prob = VProb { p2d = (ikt * sView) !> "wv",
                   cam = (subindex "c" $ map (fromMatrix Contra Co) $ selectPos selcams $ sCam s) !> "1v 2x",
                   p3d = (subindex "n" $ map (fromVector Contra) $ selectPos selpts $ sPts s) !> "1x",
                   l2d = undefined,
                   l3d = undefined  }


tensorSelect :: (Int,Int) -> [Int] -> SparseVP -> VProb
tensorSelect sz selcams s = t where
    selpts = commonPointsL s selcams
    t = normalizeCoords sz $ tensorProblem selpts selcams s

tensorSelectK :: [Int] -> SparseVP -> VProb
tensorSelectK selcams s = t where
    selpts = commonPointsL s selcams
    t = tensorProblemK selpts selcams s


commonPointsL s = foldl1' (\a b -> reverse (myintersect a b)) . map (p_of_v s)


-- | extract the elements of a dense problem in sparse format:
sparseFromTensor :: VProb -> SparseVP
sparseFromTensor p = recalibrate ks r where
    r = (goSparse obsp) {
            sPts = pts,
            sCam = cs }
    (ks,cs) = unzip $ map sepCam $ getCams p
    pts = getP3d p
    obsp = zipWith (map . (\n (v,l)->((n,v),l))) [0.. ] $ map (zip [0..] . map (toList.asVector) . flip parts "c")  . flip parts "n" . inhomogT "v" . p2d $ p



commonPoints s i j = myintersect (p_of_v s i) (p_of_v s j)

myintersect as bs = go as bs [] where
    go [] _ x = x
    go _ [] x = x
    go (a:as) (b:bs) x
        | a < b = go (a:as) bs x
        | a > b = go as (b:bs) x
        | otherwise = go as bs (a:x)


getPairs s i j | null com = Nothing
               | otherwise = Just $ compact $ outf (fromRows p) (fromRows q)
    where com = commonPoints s i j
          f k = (unitary $ ako s (k,i), unitary $ ako s (k, j))
          (p,q) = unzip $ map f com

          outf a b = fromColumns [ u*v |  u<-us, v<-vs ]
            where us = toColumns a
                  vs = toColumns b

          compact x = if rows x < 9 then x else takeRows 9 $ snd $ qr x
                                                    -- compact' 9 x

compact' n x = takeRows n $ f (trans x <> x) where
        f m = diag (sqrt (abs l)) <> trans v
            where (l,v) = eigSH' m

mkEpiObs s = [ (ij, p) | (ij,Just p) <- obs]
    where obs = [((i,j), getPairs s i j) | i <- [0 .. snC s -2], j <- [i+1 .. snC s -1]]

select [] = id
select lks = filter (flip elem lks . fst)

selectPos is = map snd . filter (flip elem is . fst) . zip [0 ..]


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

    fcost sol = 1E4 * sqrt (pnorm PNorm2 v ^ 2 / fromIntegral (dim v))
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


rangeCoords s = (rg xs, rg ys) where
    xs = map (head.snd) (lObs s)
    ys = map (last.snd) (lObs s)
    rg as = (minimum as, maximum as)

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

    fcost sol = 1E4 * sqrt (pnorm PNorm2 v ^ 2 / fromIntegral (dim v))
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

alter k = (!!k) . iterate f
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

norm v = pnorm PNorm2 (v :: Vector Double)

arrayOf xs = (A.listArray (0, length xs -1) xs A.!)

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
