{-# LANGUAGE RecordWildCards #-}

module Vision.Multiview(
    VProb(..), VPParam(..), stdprob, mkVProb, randomVProb,
    genProb, randomProb,
    fundamental, trifocal, quadrifocal,
    multiView,
    kal, autoCalibrate, calibrate, quality,
    homogT, inhomogT, eps3, eps4, unitT, frobT, normInfT,
    camerasFromTrifocalHZ, correctFundamental,
    splitEvery, pairsWith, randomTensor, addNoise,
    Seed,
    vprobFromFiles, vprobToFiles, shProb, drawPoints
)where

import Numeric.LinearAlgebra.Exterior
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra hiding ((.*),(<|>))
import Numeric.LinearAlgebra.Array.Util as Array
import Numeric.LinearAlgebra.Array.Solve
import Graphics.Plot(gnuplotpdf)
import System.Random
import Vision
import Data.List
import Control.Applicative
import Control.Monad hiding (join)
import System.Directory(doesFileExist)

-- import Debug.Trace
-- debugOK = True
-- debug m f x = if debugOK then trace (m ++ show (f x)) x else x

type Seed = Int

data VProb = VProb
    { p3d :: Tensor Double
    , cam :: Tensor Double
    , p2d :: Tensor Double
    , l3d :: Tensor Double
    , l2d :: Tensor Double }

data VPParam = VPParam
    { numCams    :: Int
    , numPoints  :: Int
    , pixelNoise :: Double
    , imageSize  :: Int
    , _numLines  :: Int
    , fov        :: Double
    , minDist    :: Double
    , maxDist    :: Double
    }

-- | Default parameters for the synthetic visual problem generator.
stdprob :: VPParam
stdprob = VPParam
    { numCams = 10
    , numPoints = 10
    , pixelNoise = 1
    , imageSize = 640
    , _numLines = 0
    , fov = 50*degree
    , minDist = 1.5
    , maxDist = 2.5 }

mkVProb :: VPParam -> Seed -> VProb
mkVProb VPParam{..} seed = r where
    [seedP, seedC1, seedC2, seedD, seedN, _seedL] = take 6 (randoms (mkStdGen seed))

    mpoints3D = uniformSample seedP numPoints [(-0.5,0.5),(-0.5,0.5),(-0.5,0.5)]
    points3D = homogT "x" $ fromMatrix Co Contra mpoints3D !"nx"

    targets  = toRows $ uniformSample seedD numCams [(-0.1,0.1),(-0.1,0.1),(-0.1,0.1)]
    cenDirs = map unitary $ toRows $ uniformSample seedC1 numCams [(-1,1),(-1,1),(-1,1)]
    cenDist = toList $ randomVector seedC2 Uniform numCams
    centers = zipWith f cenDist cenDirs
        where f s d = LA.scalar (minDist + s * (maxDist-minDist)) * d
    mcams = zipWith mkCam centers targets
        where mkCam c p = syntheticCamera $ easyCamera fov (f c) (f p) (20*degree)
                 where f v = (x,y,z) where [x,y,z] = toList v

    cams = subindex "c" $ map ((!"vx") . fromMatrix Contra Co) mcams

    views = f (cams * points3D)
        where f = homogT "v" . addNoise seedN sigma . inhomogT "v"

    r = VProb { p3d = points3D
              , p2d = views
              , cam = cams
              , l3d = undefined
              , l2d = undefined }

    pixFactor = fromIntegral imageSize/2
    sigma = pixelNoise / pixFactor

randomVProb :: VPParam -> IO VProb
randomVProb p = mkVProb p <$> randomIO


genProb :: Int -> Int -> Double -> [Seed] -> VProb
genProb n m noise [seedP, seedC, seedD, seedN] = r where
    centers = uniformSample seedC m [(-1,1),(-1,1),(-1,-0.5)]
    dirs    = uniformSample seedD m [(-0.1,0.1),(-0.1,0.1),(0.8,1)]
    mcams = zipWith tcam (toRows centers) (toRows dirs)
    mpoints3D = uniformSample seedP n [(-0.4,0.4),(-0.4,0.4),(0.4,1)]
    cams = subindex "c" $ map ((!"vx") . fromMatrix Contra Co) mcams
    points3D = homogT "x" $ fromMatrix Co Contra mpoints3D !"nx"

    seedL = seedP * seedC + seedD*seedN -- should be another seed
    mpoints3D' = uniformSample seedL n [(-0.4,0.4),(-0.4,0.4),(0.4,1)]
    points3D' = homogT "x" $ fromMatrix Co Contra mpoints3D' !"nx"
    lines3d = subindex "n" $ zipWith (/\) (parts points3D "n") (parts points3D' "n")
    lines2d = (cams!"cu1" .* cams!"cv2") * lines3d * eps3!"uvl"

    views = f noise (cams * points3D)
        where f s = homogT "v" . addNoise seedN s . inhomogT "v"

    r = VProb { p3d = points3D,
                p2d = views,
                cam = cams,
                l3d = lines3d,
                l2d = lines2d }
genProb _ _ _ _ = error $ "genProb requires four seeds"


tcam :: Vector Double -> Vector Double -> Matrix Double
tcam c p = syntheticCamera $ easyCamera (60*degree) (f c) (f p) (20*degree)
   where f v = (x,y,z) where [x,y,z] = toList v



randomProb :: Int -> Int -> Double -> IO VProb
randomProb n m noise = genProb n m noise <$> replicateM 4 randomIO

-- Homogeneous and inhomogenous versions of a tensor in a given index:

inhomogT :: Name -> Tensor Double -> Tensor Double
inhomogT n t = (init `onIndex` n) t / last (parts t n)

homogT :: Name -> Tensor Double -> Tensor Double
homogT n t = ((++[1]) `onIndex` n) t

-- Comparation of homogeneous transformations with the same scale:

unitT :: Tensor Double -> Tensor Double
unitT t = t / Array.scalar (frobT t)

frobT :: Tensor Double -> Double
frobT = pnorm PNorm2 . coords

normInfT :: Tensor Double -> Double
normInfT t = pnorm Infinity . coords $ t


-- Very frequently used tensors:

eps3 :: Tensor Double
eps3 = cov     (leviCivita 3)

eps4 :: Tensor Double
eps4 = contrav (leviCivita 4)

-- The multiview tensors:

-- We assume in all cases that the image index (dim 3) is lower than the world index (dim 4).
-- It is ok when we rename parts of a problem, we get "1x" etc.

fundamental :: Tensor Double -> Tensor Double -> Tensor Double
fundamental m n = unitT $
  eps3!"rpq" * m!"pa" * m!"qb" * eps4!"abcd" *
  n!"sc" * n!"td" * eps3!"stk"

trifocal :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double
trifocal m n p = unitT $
  eps4!"abcd" * m!"ia" * n!"jb" *p!"pc" * p!"qd" * eps3!"pqk"

quadrifocal :: Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double -> Tensor Double
quadrifocal a b c d = unitT $
  eps4!"ijlk" * a!"pi" * b!"qj" * c!"rk" * d!"sl"

-- Other tools:

addNoise :: Seed -> Double -> Tensor Double -> Tensor Double
addNoise seed sigma t = t + Array.scalar sigma .* noise
    where noise = mapArray (const $ randomVector seed Gaussian (dim $ coords t)) t



shcam :: Matrix Double -> [[Double]]
shcam p = c where
   (h,f) = toCameraSystem p
   c = ht (h <> diag (fromList [1,1,1,5])) (cameraOutline f)

drawCameras :: String -> [Matrix Double] -> [[Double]] -> IO ()
drawCameras tit ms pts = do
  let cmd = map (f.shcam) ms
      f c = (c,"notitle 'c1' with lines 1")

  gnuplotpdf tit
         (  "set view 72,200; "
         ++ "set xlabel '$x$'; set ylabel '$y$'; set zlabel '$z$';"
         ++ "set size ratio 1;"
         ++ "set notics;"
         ++ "splot ")
         (cmd ++ [(pts,"notitle 'v' with points 3")])

shProb :: String -> VProb -> IO ()
shProb tit p = drawCameras tit
    (map asMatrix $ parts (cam p) "c")
    (toLists $ asMatrix $ inhomogT "x" $ p3d p)

drawPoints :: [[Double]] -> [[Double]] -> IO ()
drawPoints pts1 pts2 = do
  gnuplotpdf "points"
         (  "set size square; set title \"The first view\";"
         ++ "set xlabel '$x$'; set ylabel '$y$'; "
      --   ++ "set notics;"
         ++ "plot [-1:1] [-1:1] ")
         [(pts1,"title 'true' with points 3"),
          (pts2,"title 'observed' with points 1")]


correctFundamental :: Matrix Double -> Matrix Double
correctFundamental f = f' where
   (u,s,v) = svd f
   s1:s2:_ = toList s
   f' = u <> diag (fromList [s1,s2,0.0]) <> trans v


-- The algorithm in Hartley and Zisserman
camerasFromTrifocalHZ :: Tensor Double -> (Tensor Double, Tensor Double, Tensor Double)
camerasFromTrifocalHZ tri = (t1,t2,t3) where
    p3 = cameraAtOrigin
    e1 = unitT $ epitri tri
    e2 = unitT $ epitri (tri!>"jik") -- hmmm!!!
    p1 = (asMatrix $ tri!>"ijk"* cov e2!"j") & asColumn (coords e1)    -- (e in l slot, in HZ)
    p2 = (asMatrix (e2!"i"*e2!"j") - ident 3)
          <> (asMatrix $ tri!>"ijk"* cov e1!"i") & asColumn (coords e2)
    f = unitT . fromMatrix Contra Co
    t1 = f p1 !"1x"
    t2 = f p2 !"2x"
    t3 = f p3 !"3x"
    a & b = fromBlocks[[a,b]]

-- image on c1 of the center of c3
-- This method should only be used on geometrically correct trifocal tensors
epitri :: Tensor Double -> Tensor Double
epitri tri = epi where
     epi = lin tri (vector[0,0,1]) !"a"
           * lin tri (vector[1,0,0])!"b" * (contrav eps3)!"abc"
     lin t p = linQuad t * p!"k" * p!"K"
     linQuad t = (t!>"ijk") * vector[0,0,1]!"a"
                            * eps3!"ajJ" * (t!>"IJK") * eps3!"iIr"


--------------------------------------------------------------------------


--fixRot m = p where
--   (_,r,c) = factorizeCamera m
--   p = r <|> -r<>c

kal :: Tensor Double -> Tensor Double
kal cs = mapTat ((!>"xw").applyAsMatrix getk) ["c"] cs
    where getk m = k where (k,_,_) = factorizeCamera m

-- ks = kal . cam

quality :: VProb -> Double
quality prob = sqrt $ frobT (inhomogT "v" (p2d prob) - inhomogT "v" proj) ^(2::Int) / (n*m*2)
    where
        proj = cam prob * p3d prob
        n = fromIntegral $ size "n" (p2d prob)
        m = fromIntegral $ size "c" (p2d prob)


calibrate :: VProb -> VProb
calibrate p = p { cam = newcams!>"wv", p2d = newp2d!>"wv" } where
    ks = parts (kal $ cam p) "c"
    iks = map ((!"wv").applyAsMatrix inv) ks
    cs = parts (cam p) "c"
    ps = parts (p2d p) "c"
    newcams = subindex "c" $ zipWith (*) iks cs
    newp2d  = subindex "c" $ zipWith (*) iks ps

------------------------------------------------------------------------

vprobToFiles :: FilePath -- ^ cameras
             -> FilePath -- ^ images
             -> FilePath -- ^ points
             -> FilePath -- ^ calibration
             -> Tensor Double -- ^ calibration
             -> VProb
             -> IO ()
vprobToFiles fc fi fp fk k p = do
     let fmt = tail . snd . break (=='\n') . dispf 5
         matcams = fromBlocks . map return . map asMatrix . flip parts "c" . cam $ p
         matviews = fromBlocks . return . map asMatrix . flip parts "c" . inhomogT "v" .  p2d $ p
         matpts = asMatrix . inhomogT "x" .  p3d $ p
         matkal = fromBlocks . map (return . asMatrix) . flip parts "c" $ k
     writeFile fc (fmt matcams)
     writeFile fi (fmt matviews)
     writeFile fp (fmt matpts)
     writeFile fk (fmt matkal)

vprobFromFiles :: FilePath   -- ^ cameras (may not exist)
               -> FilePath   -- ^ images (required)
               -> FilePath   -- ^ points (may not exist)
               -> IO VProb
vprobFromFiles fc fi fp = do
    okCams <- doesFileExist fc
    okPts  <- doesFileExist fp
    matviews <- loadMatrix fi
    matcams  <- if okCams then loadMatrix fc else return 1
    matpts   <- if okPts  then loadMatrix fp else return 1
 -- matkal   <- loadMatrix (filename++".kal.txt")
    let cs = if okCams
                then subindex "c" . map ((!>"1v 2x") . fromMatrix Contra Co . fromRows) . splitEvery 3 . toRows $ matcams
                else error $ fc ++ " not found!"
     -- ks = subindex "c" . map ((!>"1v 2w") . fromMatrix Contra Co . fromRows) . splitEvery 3 . toRows $ matkal
        vw = homogT "v" . subindex "c" . map ((!>"1n 2v") . fromMatrix Co Contra . fromColumns) . splitEvery 2 . toColumns $ matviews
        ps = if okPts
                then homogT "x" . (!>"1n 2x") . fromMatrix Co Contra $ matpts
                else error $ fp ++ " not found!"
    return VProb { cam = cs, p2d = vw, p3d = ps, l2d = undefined, l3d = undefined }

-----------------------------------------------------------------------


multiView :: Int -- ^ 2, 3, or 4 bootstraping views
          -> ALSParam Variant Double
          -> Seed
          -> Tensor Double -- ^ views c v n
          -> VProb
multiView k param seed vs =
    VProb { cam = allcams, p3d = allp3d, p2d = vs,
            l3d= undefined, l2d= undefined } where
    vs2 = (take k `onIndex` "c") vs
    views = renameParts "c" vs2 "v" ""
    dat = outers views

    sol = extractCams param seed dat

    cs = subindex "c" (map (!"vx") sol)
    ps = solveP cs vs2 "v"
    allcams = solveP ps vs "v"
    allp3d  = ps --solveP allcams vs "v"


extractCams :: ALSParam Variant Double
            -> Seed
            -> Tensor Double -- ^ outer products of the views
            -> [Tensor Double]
extractCams param seed dat | order dat -1 == 2 = [f1,f2]
                           | order dat -1 == 3 = [t1,t2,t3]
                           | order dat -1 == 4 = [q1,q2,q3,q4]
                           | otherwise = error $ "extractCams requires 2, 3, or 4 views"
    where solver = mlSolve param [eps4!"pqrs"] (init4Cams seed)
          fun = solveH dat "12"
          ([f1,_,f2,_],_errf) = solver (fun !>"1y 2x" * contrav eps3!"12y" * contrav eps3!"34x")
          tri = solveH (dat * eps3!"1pq" * eps3!"2rs") "pr3" !> "p1 r2 3x"
          ([t1,t2,t3,_],_errt) = solver (tri* contrav eps3!"34x")
          qua = solveH (dat * eps3!"1ab" * eps3!"2fg" * eps3!"3pq" * eps3!"4uv") "afpu"
          ([q1,q2,q3,q4],_errq) = solver (qua!"1234")

init4Cams :: Seed -> [Tensor Double]
init4Cams seed = [ic1!"1p",ic2!"2q",ic3!"3r",ic4!"4s"]
     where [ic1,ic2,ic3,ic4] = parts (randomTensor seed [-4,3,-4]) "1"


pairsWith :: (b -> b -> a) -> [b] -> [a]
pairsWith _ [] = []
pairsWith _ [_] = []
pairsWith f (x:xs) = map (f x) xs ++ pairsWith f xs


splitEvery :: Int -> [t] -> [[t]]
splitEvery _ [] = []
splitEvery k l = take k l : splitEvery k (drop k l)


randomTensor :: Seed -> [Int] -> Tensor Double
randomTensor seed dms = listTensor dms cs where
   g = mkStdGen seed
   cs = randomRs (-1,1) g


-----------------------------------------------------------------------------------

-- | Linear autocalibration for diag(f,f,1) based on the absolute dual quadric.
autoCalibrate :: Maybe Double -> VProb -> VProb
autoCalibrate Nothing  = autoCalibrateAux sel
autoCalibrate (Just f) = autoCalibrateAux (sel3 f)

-- linear autocalibration for diag f1 f2 1 based on the absolute dual quadric
autoCalibrateAux :: Tensor Double -> VProb -> VProb
autoCalibrateAux t p = p { p3d = (p3d p * hi)!>"yx", cam = cs' !> "yx"}
 where
   cs = cam p
   cs' = cs * h
   hi = applyAsMatrix inv h !"yx"
   a = (map (\m-> m * m!"uy") `onIndex` "c") cs
   q = solveH (a*t) "xy"
   h = setType "y" Co $ applyAsMatrix psqrt q


psqrt :: Matrix Double -> Matrix Double
psqrt m | defpos = v <> diag (sqrt s')
        | defneg = psqrt (-m)
        | otherwise = --debug "NODEFPOS eig = " (const s') $ 
                      ident 4

       where (s,v) = eigSH' m
             s' = fromList $ init (toList s) ++ [1]
             [s1,s2,s3,s4] = toList s
             defpos = s3>0 && abs s3 > abs s4
             defneg = s2<0 && abs s2 > abs s1


sel3 :: Double -> Tensor Double
sel3 f = (!"ruv") . cov . mkAssoc [8,3,3] . (dg++) . map (flip (,) 1) $
       [ [0,0,1], [1,0,2], [2,1,2],
         [3,1,0], [4,2,0], [5,2,1] ]
  where dg = [([6,0,0],2),([6,1,1],-2),
              ([7,0,0],2),([7,2,2],-2*f*f)]

sel :: Tensor Double
sel = (!"ruv") . cov . mkAssoc [7,3,3] . (dg++) . map (flip (,) 1) $
      [ [0,0,1], [1,0,2], [2,1,2],
        [3,1,0], [4,2,0], [5,2,1] ]
  where dg = [([6,0,0],2),([6,1,1],-2)]

-------------------------------------------------------------------------

