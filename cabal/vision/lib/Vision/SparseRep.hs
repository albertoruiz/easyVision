{-# OPTIONS -fno-warn-unused-do-bind #-}

module Vision.SparseRep(
    SparseVP(..), loadSVP, Epi(..),
    sparseFromTensor, tensorSelectK, tensorProblemK, recalibrate,
    commonPoints, ptsVisibleBy, lengthTracks, viewedPoints,
    kError, detailedErrors, sGError, sEError, sEEError, sEAError, objectQualityS, poseQualityS,
    signalLevelS, infoSProb, pixelRange, essentials,
    trackMatrix, trackMissing
) where

import Numeric.LinearAlgebra as LA hiding(i)
import qualified Data.Array as A
import Numeric.LinearAlgebra.Tensor
import Numeric.LinearAlgebra.Array.Util
import Data.List
import Vision.Camera
import Vision.Stereo
import Util.Estimation(estimateHomography)
import Util.Homogeneous
import Util.Quaternion
import Vision.TensorRep
import Vision.Tensorial
import Text.Printf
import Util.Misc(splitEvery, arrayOf, myintersect', impossible,
                 mean, sqr, median, shDist, vec, Mat, degree, unitary)
import Util.Covariance(correlation)

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
                    , epiObs :: [((Int,Int), Epi)] -- reduced measurement matrix for each pair of views
                    , ako :: (Int, Int) -> Vector Double -- fast access to calibrated observations
                    }

-----------------------------------------------------------------------------------

loadRawTracks :: FilePath -> IO ([Vector Double], [[((Int, Int), [Double])]])
loadRawTracks filepts = do
    s <- (filter (('#'/=).head.head) . map words . lines) `fmap` readFile filepts
    let pts = map ((fromList . (++[1])) . map read . take 3) s :: [Vector Double]
        g n [v,x,y] = ((n ::Int,read v :: Int), [read x ,read y :: Double])
        g _ _ = impossible "loadRawTracks"
        obsp = zipWith (map . g) [0..] $ map (splitEvery 3 . drop 4) s
    return (pts, obsp)

goSparse :: [[((Int, Int), [Double])]] -> SparseVP
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
                                                                f _ _ = impossible "goSparse"



-- | updates sKal, ako and epiObs
recalibrate :: [Mat] -> SparseVP -> SparseVP
recalibrate ks s = s' {epiObs = mkEpiObs s'} where
    s' = s { sKal = ks, ako = ako'} 
    ik = arrayOf (map inv ks)
    ako' = (A.accumArray f (fromList [0,0,0]) ((0,0),(snP s -1,snC s-1)) obs' A.!)
        where f _ ((_p,v),[x,y]) = ik v <> fromList [x,y,1]
              f _ _ = impossible "recalibrate"
              obs' = map (\(a,b)-> (a,(a,b))) (lObs s)


-- | Creates a sparse visual problem, from raw data in a file with SBA format
loadTracks :: FilePath -> IO SparseVP
loadTracks filepts = do
    (pts,obsp) <- loadRawTracks filepts
    return (goSparse obsp) { sPts = pts }

-- | load a sparse visual problem with cameras and calibration info from files with SBA format
loadSVP :: FilePath -> FilePath -> FilePath -> IO SparseVP
loadSVP filepoints filecams filecalib = do
    s <- loadTracks filepoints
    cams <- loadQCams filecams
    k <- loadMatrix filecalib
    let one = rows k == 3
        ks | one = (replicate (snC s) k)
           | otherwise = map head (toBlocksEvery 3 3 k)
    return $ recalibrate ks s { sCam = cams }



loadQCams :: FilePath -> IO [Matrix Double]
loadQCams filecams = (map (camFromQuat . map read . words) . lines) `fmap` readFile filecams
  where
    camFromQuat [s, a, b, c, x, y, z] = camera
        where q = Quat s (fromList [a,b,c])
              r = quatToRot q
              cen = fromList [x,y,z]
              camera = fromBlocks [[r, asColumn cen]]
    camFromQuat _ = impossible "loadQCams"
    
-------------------------------------------------------------

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



tensorSelectK :: [Int] -> SparseVP -> VProb
tensorSelectK selcams s = t where
    selpts = commonPointsL s selcams
    t = tensorProblemK selpts selcams s

commonPointsL :: SparseVP -> [Int] -> [Int]
commonPointsL s = foldl1' (\a b -> reverse (myintersect' a b)) . map (p_of_v s)


-- | extract the elements of a dense problem in sparse format:
sparseFromTensor :: VProb -> SparseVP
sparseFromTensor p = recalibrate ks r where
    r = (goSparse (map remo obsp)) {
            sPts = pts,
            sCam = cs }
    (ks,cs) = unzip $ map sepCam $ getCams p
    pts = getP3d p
    obsp = zipWith (map . (\n (v,l)->((n,v),l))) [0.. ] $ map (zip [0..] . map (toList.asVector) . flip parts "c")  . flip parts "n" . p2d $ p
    remo zs = [(loc, [x/w,y/w]) | (loc,[x,y,w]) <- zs, w/= 0.0]

commonPoints :: SparseVP -> Int -> Int -> [Int]
commonPoints s i j = myintersect' (p_of_v s i) (p_of_v s j)


-- compute M^
getPairs :: SparseVP
    -> Int
    -> Int
    -> Maybe (Matrix Double,[Int])
getPairs s i j | null ks = Nothing
               | otherwise = Just (compact' $ outf (fromRows p) (fromRows q), ks)
    where ks = commonPoints s i j
          f k = (unitary $ ako s (k,i), unitary $ ako s (k, j))
          (p,q) = unzip $ map f ks

          outf a b = fromColumns [ u*v |  u<-us, v<-vs ]
            where us = toColumns a
                  vs = toColumns b

          compact x = if rows x < 9 then x else takeRows 9 $ snd $ qr x
          compact' x = takeRows (rows x `min` 9) c
             where (l,v) = eigSH' (trans x <> x)
                   c = diag (sqrt (abs l)) <> trans v


-- more info about a view pair
data Epi = Epi { m_hat :: Mat,
                 com  :: [Int], -- index of common points
                 esen :: Mat,
                 nEpi :: Int,   -- lenght of com
                 s2 :: Double,
                 s7 :: Double,
                 p_homog :: Mat,    -- planar homography
                 h_err :: Double,   -- its error
                 rot :: Maybe Mat } -- promising rotation

prepEpi :: SparseVP -> ((Int, Int), (Mat,[Int])) -> ((Int, Int), Epi)
prepEpi s ((i,j),(m,ks)) = ((i,j),
    Epi { m_hat = m,
          com = ks,
          esen = e,
          nEpi = length ks,
          s2 = fst (qEssen e),
          s7 = sumElements (subVector 6 3 sm) / sumElements (subVector 0 6 sm),
          p_homog = h, h_err = he,
          rot = mbrot } ) where
    ebad = linearEssen m
    ps  = map (\k-> toList $ inHomog $ ako s (k,i)) ks
    ps' = map (\k-> toList $ inHomog $ ako s (k,j)) ks
    egood = trans $ estimateFundamental ps ps'
    h = estimateHomography ps ps'
    he = sqrt $ pnorm Frobenius (fromLists ps - fromLists (ht h ps')) ** 2 / fromIntegral (length ks)
    e = if length ks > 8 then egood else ebad
    sm = singularValues m
    mbrot = rotOfCam `fmap` m'
        where ms = camerasFromEssential e
              m' = selectCamera' (head ps) (head ps') cameraAtOrigin ms



mkEpiObs :: SparseVP -> [((Int, Int), Epi)]
mkEpiObs s = [ prepEpi s (ij, p) | (ij,Just p) <- obs]
    where obs = [((i,j), getPairs s i j) | i <- [0 .. snC s -2], j <- [i+1 .. snC s -1]]


selectPos :: (Num a, Enum a) => [a] -> [b] -> [b]
selectPos is = map snd . filter (flip elem is . fst) . zip [0 ..]

ptsVisibleBy :: SparseVP -> [Int] -> [Int]
ptsVisibleBy s sel = reverse $ map head . filter ((>1).length) $ group $ sort $ concatMap (p_of_v s) sel



lengthTracks :: SparseVP -> [Int]
lengthTracks s = map (length . v_of_p s) $  [0 .. snP s -1]

-- | number of points visible in each image
viewedPoints :: SparseVP -> [Int]
viewedPoints s = map (length . p_of_v s) $  [0 .. snC s -1]



---------------------------------------------------------------------

-- | geometric reprojection error for view k, in calibrated units
kErrorView :: Int -> SparseVP -> (Double,Double)
kErrorView k s = (2*e2, 1000*sqrt e2) where
    obs = filter ((==k).snd) $ map fst (lObs s)
    n = fromIntegral (length obs)
    c = arrayOf (sCam s)
    p = arrayOf (sPts s)
    g ac (i,j) = ac + sqr(x/z-a/w) + sqr(y/z-b/w)
        where [a,b,w] = toList $ c j <> p i
              [x,y,z] = toList (ako s (i,j))
    e2 = foldl' g 0 obs / (2*n)

-- | kError in each view
detailedErrors :: SparseVP -> [(Int,Double)]
detailedErrors s = map f [0..length (sCam s)-1] where
    f k = (k, snd (kErrorView k s))


-- | geometric reprojection error of a sparse visual problem
--   in calibrated units
kError :: SparseVP -> (Double,Double)
kError s = (2*e2, 1000*sqrt e2) where
    obs = map fst (lObs s)
    n = fromIntegral (length obs)
    c = arrayOf (sCam s)
    p = arrayOf (sPts s)
    g ac (i,j) = ac + sqr(x/z-a/w) + sqr(y/z-b/w)
        where [a,b,w] = toList $ c j <> p i
              [x,y,z] = toList (ako s (i,j))
    e2 = foldl' g 0 obs / (2*n)


-- | geometric reprojection error of a sparse visual problem in pixel units
sGError :: SparseVP -> (Double,Double)
sGError s = (2*e2, sqrt e2) where
    obs = lObs s
    n = fromIntegral (length obs)
    c = arrayOf (zipWith (<>) (sKal s) (sCam s))
    p = arrayOf (sPts s)
    g ac ((i,j), [x,y]) = ac + sqr(x-a/w) + sqr(y-b/w)
        where [a,b,w] = toList $ c j <> p i
    g _ _ = impossible "sGError"
    e2 = foldl' g 0 obs / (2*n)

-- | geometric epipolar error of a sparse visual problem
-- (sum of mean distances to epipolar lines induced by the
-- estimated (corrected) images of the estimated points).
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

distE2 :: [Double] -> [Double] -> Double
distE2 [x,y] [l1,l2,l3] = sqr(l1*x + l2*y + l3)/(sqr l1 + sqr l2)
distE2 _ _ = impossible "distE2"

-- | geometric empiric epipolar error of a sparse visual problem
-- (sum of mean distances to epipolar lines induced by the
-- observed corresponding points).
sEEError :: SparseVP -> (Double,Double)
sEEError s = (2*e2, sqrt e2) where
    obs = lObs s
    n = fromIntegral (length obs)
    c = arrayOf (zipWith (<>) (sKal s) (sCam s))
--    cen = arrayOf (map (nullVector) (sCam s))
--    ic = arrayOf (map (pinv) (sCam s))
--    p = arrayOf (sPts s)
    e2 = foldl' g 0 obs / (2*n)

    lEpi v j w = toList $ unitary $ fundamentalFromCameras (c w) (c v) <> (fromList (aObs s A.!  (j,w)))

--    lEpi'' v j w = toList $ unitary $ fundamentalFromCameras (c w) (c v) <> (c w <> p j)

    g ac ((j,v), x) = ac + mean [ distE2 x (lEpi v j w) | w <- v_of_p s j \\ [v]]



-- | empiric algebraic epipolar error used by GEA
--
-- (we should?) replicate value from pairs, as used in GEA
sEAError :: SparseVP -> (Double,Double)
sEAError s = (2*e2, sqrt e2) where
    obs = lObs s
    n = fromIntegral (length obs)
    ik = arrayOf (map inv $ sKal s)
    c = arrayOf (zipWith (<>) (sKal s) (sCam s))
--    cen = arrayOf (map (nullVector) (sCam s))
--    ic = arrayOf (map (pinv) (sCam s))
--    p = arrayOf (sPts s)
    e2 = foldl' g 0 obs / (2*n)

    lEpi v j w = toList $ unitary $ ik w <> (fundamentalFromCameras (c w) (c v) <> (fromList (aObs s A.!  (j,w))))

    g ac ((j,v), x) = ac + mean [ distAlgE2 x' (lEpi v j w) | w <- v_of_p s j \\ [v]]
        where [x'] = ht (ik v) [x]

distAlgE2 :: [Double] -> [Double] -> Double
distAlgE2 [x,y] [l1,l2,l3] = sqr(l1*x + l2*y + l3)
distAlgE2 _ _ = impossible "distAlgE2"
---------------------------------------------------------------

objectQualityS :: SparseVP -> SparseVP -> Double
objectQualityS = metricConsis (map inHomog . sPts)

poseQualityS :: SparseVP -> SparseVP -> Double
poseQualityS = metricConsis (map (inHomog.nullVector) . sCam)

----------------------------------------------------------------

infoSProb :: SparseVP -> IO ()
infoSProb s = do
    let ((x1,x2),(y1,y2)) = pixelRange s
        f = median $ map getFK (sKal s)
    printf "Pixel Range: (%.1f, %.1f) (%.1f, %.1f)\n" x1 x2 y1 y2
    putStr "K0: "; putStr. dispf 2 $ sKal s !! 0
    printf "effective FOV = %.2f\n" $ 2 * atan2 (min (x2-x1) (y2-y1)/2) f / degree
    let empiricRcond = 1/0.65
    printf "cameras       = %d\n" (snC s)
    printf "tracks        = %d\n" (snP s)
    printf "views         = %d\n" (length (lObs s))
    shDist "track length  = " "%.1f" "%.0f" (map fromIntegral $ lengthTracks s)
    shDist "points/view   = " "%.1f" "%.0f" (map fromIntegral $ viewedPoints s)
    correl 10 s
    let obs = epiObs s
        fullobs = filter ((>8).rows.m_hat.snd) obs
    printf "pairs         = %d tot, %d >8\n" (length obs) (length fullobs )
    let noisedist = map ((\r -> -logBase 10 r). rcond.m_hat.snd) fullobs
        noise = 10**(-median noisedist)
        sl  = signalLevelS s
    shDist "rcond         = " "%.1f" "%.1f" noisedist
    printf "signal        = %.1f\n" sl
    printf "typ f         = %.1f\n" f
    printf "est sigma     = %.1f\n" (noise*f*empiricRcond)
    printf "est N/R       = %.1f %%\n" (100*noise*f*empiricRcond/sl)
    pairQuality 8 s

-- in raw coordinates
signalLevelS :: SparseVP -> Double
signalLevelS s = mean . map (sqrt . minElement . eigenvaluesSH' . snd . meanCov . fromLists . visible) $ [0..snC s -1]
    where visible v = map (init . (flip $ curry (aObs s A.!)) v) (p_of_v s v)

pixelRange :: SparseVP -> ((Double, Double), (Double, Double))
pixelRange p = (rangeL x, rangeL y) where
    [x,y] = transpose $ map snd (lObs p)
    rangeL zs = (minimum zs, maximum zs)



-- compute the essential matrix of the pair
-- from the reduced measurement matrix M^
linearEssen :: Mat -> Mat
linearEssen = trans . reshape 3 . last . toColumns . snd . rightSV


-- | estimated essential matrix and common points of each pair
essentials :: SparseVP -> [((Int,Int),(Mat,[Int]))]
essentials s = map f . epiObs $ s where
    f ((i,j),m) = ((i,j),(esen m, com m)) where

pairQuality :: Int -> SparseVP -> IO ()
pairQuality n p = do
    let q = unzip $ map (qEssen.fst.snd) $ filter ((>=n).length.snd.snd) $ essentials p
        q2 = filter ((>n).nEpi.snd) $ (epiObs p)
    shDist "s2:  " "%.3f" "%.3f" $ fst $ q
--    shDist "he:  " "%.1f" "%.1f" $ map (negate . logBase 10) $ snd $ q
    shDist "s7:  " "%.1f" "%.1f" $ map (negate . logBase 10)$ map (s7.snd) $ q2
    shDist "he:  " "%.1f" "%.1f" $ map (negate . logBase 10)$ map (h_err.snd) $ q2

correl :: Int -> SparseVP -> IO ()
correl n p = do
    putStr "correlation n s2 s3 s7 s9 he "
    putStr . dispf 1 . correlation . snd . meanCov $ q
    shDist "pairwise n: " "%.1f" "%.0f" ns
  where
    q = fromLists $ map (f.snd) $ filter ((>=n).nEpi.snd) $ es
    ns = map (fromIntegral.nEpi.snd) es
    es = epiObs p where
    f x = [fromIntegral k,s2',s3, s7 x, ss@>8/ss@>0, h_err x]
        where k = nEpi x
              (s2',s3) = qEssen (esen x)
              ss = singularValues (m_hat x)

--------------------------------------------------------------

trackMatrix :: SparseVP -> Mat
trackMatrix p = fromBlocks [[asColumn (ako p (i,j)) | i<-[0..length (sPts p) -1]]| j <- [0.. length(sCam p)-1]]

trackMissing :: SparseVP -> Mat
trackMissing p = fromBlocks [[ g (ako p (i,j)) | i<-[0..length (sPts p) -1]]| j <- [0.. length(sCam p)-1]]
    where g v = if v == z then 0 else 1
          z = vec [0,0,0]
