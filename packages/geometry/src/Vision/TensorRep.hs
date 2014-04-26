{-# LANGUAGE RecordWildCards #-}

module Vision.TensorRep(
    VProb(..), VPParam(..), stdprob, mkVProb, randomVProb, mkHelix,
    genProb, randomProb, getCams, getP3d,
    randomTensor, addNoise,
    Seed,
    vprobFromFiles, vprobToFiles, shProb, drawPoints,
)where

import Numeric.LinearAlgebra.Exterior
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util(unitary)
import Numeric.LinearAlgebra.Array.Util as Array
import Numeric.LinearAlgebra.Util(gnuplotpdf)
import System.Random
import Vision.Camera
import Control.Applicative
import Control.Monad hiding (join)
import System.Directory(doesFileExist)
import Util.Misc(splitEvery,degree)


type Seed = Int

data VProb = VProb
    { p3d :: Tensor Double
    , cam :: Tensor Double
    , p2d :: Tensor Double
    , l3d :: Tensor Double
    , l2d :: Tensor Double
    }

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

------------------------------------------------------------

mkHelix :: VPParam -> Seed -> VProb
mkHelix VPParam{..} seed = r where
    [seedP, _, _, seedD, seedN, _seedL] = take 6 (randoms (mkStdGen seed))

    mpoints3D = uniformSample seedP numPoints [(-0.5,0.5),(-0.5,0.5),(-0.5,0.5)]
    points3D = homogT "x" $ fromMatrix Co Contra mpoints3D !"nx"

    targets  = toRows $ uniformSample seedD numCams [(-0.1,0.1),(-0.1,0.1),(-0.1,0.1)]
--    cenDirs = map unitary $ toRows $ uniformSample seedC1 numCams [(-1,1),(-1,1),(-1,1)]
--    cenDist = toList $ randomVector seedC2 Uniform numCams
    centers = [fromList [x t,y t,z t] | t <- (id $ toList $ linspace numCams (0,2*pi))]
        where x t = minDist * cos t
              y t = minDist * sin t
              z _t = 0
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


-- Homogeneous and inhomogenous versions of a tensor in a given index:

inhomogT :: Name -> Tensor Double -> Tensor Double
inhomogT n t = (init `onIndex` n) t / last (parts t n)

homogT :: Name -> Tensor Double -> Tensor Double
homogT n t = ((++[1]) `onIndex` n) t

-- Comparation of homogeneous transformations with the same scale:



-- Very frequently used tensors:

eps3 :: Tensor Double
eps3 = cov     (leviCivita 3)


-- Other tools:

addNoise :: Seed -> Double -> Tensor Double -> Tensor Double
addNoise seed sigma t = t + Array.scalar sigma .* noise
    where noise = mapArray (const $ randomVector seed Gaussian (dim $ coords t)) t


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



randomTensor :: Seed -> [Int] -> Tensor Double
randomTensor seed dms = listTensor dms cs where
   g = mkStdGen seed
   cs = randomRs (-1,1) g


-----------------------------------------------------------------------------------



getCams :: VProb -> [Matrix Double]
getCams p = map asMatrix $ parts (cam p) "c"

getP3d :: VProb -> [Vector Double]
getP3d p = toRows $ asMatrix $ p3d p
