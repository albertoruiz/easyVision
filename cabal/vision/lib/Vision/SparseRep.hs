module Vision.SparseRep(
    SparseVP(..), loadSVP,
    sparseFromTensor, tensorSelectK, tensorProblemK,
    commonPoints
) where

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
import Util.Sparse
import Data.Function(on)
import Data.Maybe(fromJust)
import Util.Misc(splitEvery, mean, debug, norm, arrayOf, myintersect)

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


-- loadLinks :: FilePath -> IO [(Int,Int)]
-- loadLinks filelinks = (map (l2p.words) .lines) `fmap` readFile filelinks
--     where l2p [a,b] = (read a , read b)

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


rangeCoords s = (rg xs, rg ys) where
    xs = map (head.snd) (lObs s)
    ys = map (last.snd) (lObs s)
    rg as = (minimum as, maximum as)

--------------------------------------------------------------
-- norm v = pnorm PNorm2 (v :: Vector Double)

