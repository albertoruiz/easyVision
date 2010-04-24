-- {-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances #-}

import Numeric.LinearAlgebra.Exterior
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra hiding ((.*), scalar)
import Numeric.LinearAlgebra.Array.Util as Array
-- import Numeric.LinearAlgebra.Array.Decomposition
import Numeric.LinearAlgebra.Array.Solve
-- import Graphics.Plot
import System.Random
-- import Debug.Trace
import Vision
import Vision.Multiview
import System
import Data.List
-- import qualified Data.Map as Map
import Data.Function(on)
import Control.Monad hiding (join)
import Control.Applicative hiding ((<|>))
import Text.Printf
-- import Numeric.GSL.Minimization
-- import Numeric.GSL.Differentiation
-- import Numeric.GSL.Fitting
-- import Bundle hiding (alter)
-- import Epipolar
import System.CPUTime
-- import System.IO
-- import Util.Sparse
-- import Data.Maybe

--import qualified Data.Array as A
--import qualified Data.Vector as V



import Debug.Trace

debug msg f x = trace (msg ++ show (f x)) x

debugT t = trace (formatFixed 5 t) t

time act = do
    --putStr (msg++" ")
    t0 <- getCPUTime
    act -- `seq` putStr " "
    t1 <- getCPUTime
    printf "%6.2f s CPU\n" $ (fromIntegral (t1 - t0) / (10^12 :: Double)) :: IO ()
    return ()


sh t = putStr . shN $ t
shN t = formatFixed 2 $ t  -- for use at this document
shp t = putStrLn . formatFixed 5 $ t

-- We generate a pseudorandom testing configuration with EVAL(n) points, EVAL(m) cameras, and a very small amount of gaussian image noise with standard deviation EVAL(inPix 2 sigma). We assume that the image resolution is 640 pixels, although we use normalized image coordinates from $-1$ to $+1$ to improve numerical stability.

n = 1000
m = 20

pixNoise = 1
pixFactor = 640/2
sigma = pixNoise / pixFactor
inPix d v = printf ("%."++show (d::Int)++"f pixels") (v * pixFactor)
seeds = [5555,7777,9999,2222]

prob = genProb n m sigma seeds

-- \perform{shProb "cameras" prob}

-- \perform{drawPoints [] (toLists $ asMatrix $ head $ flip parts "c" $ inhomogT "v" $ p2d prob)}


mean l = sum l / fromIntegral (length l)
median l = sort l !! (div (length l) 2)

dstmat pts = pairsWith dist pts where
  dist u v = pnorm PNorm2 (u-v)

metricConsis f = on dist (nor. dstmat .f) where
  nor x = fromList x / LA.scalar (mean x)
  dist u v = pnorm Infinity (u-v)

ihPoints3D = map coords . flip parts "n" . inhomogT "x" . p3d

simil3D' = metricConsis ihPoints3D

cameraCenters = map (coords . inhomogT "x" . flip solveH "x") . flip parts "c" . cam

poseQuality = metricConsis cameraCenters

simil3D a b = printf "%.1f%%\n" $ 100 * simil3D' a b


refine p = p { p3d = newpoints, cam = newcams }
    where ([newcams,newpoints], err) = mlSolveP param [1] [cam p, p3d p] (p2d p) "v"


fourViews = multiView 4 param
threeViews = multiView 3 param
twoViews = multiView 2 param

-- from kal . cam
getFs = map ((/2) . sum . toList . subVector 0 2 . takeDiag . asMatrix) . flip parts "c"



fixCal = subindex "c" . map ((!>"1v 2w") . fromMatrix Contra Co . kgen) . getFs

autoMetric p = frobT (k - fixCal k) / fromIntegral (5 * size "c" k)
     where k = kal . cam $ p



co = fromMatrix Contra Co cameraAtOrigin

param = defaultParameters {post = eqnorm}

shl l = concat $ intersperse ", " $ map (printf "%.1g") $ l


disp = putStr . dispf 5

-----------------------------------------------------------------


rangecoord p = ((vectorMin x, vectorMax x),(vectorMin y, vectorMax y)) where
    [x,y] = toRows $  fibers "v" (inhomogT "v" $ p2d p)

normalizeCoords (szx,szy) p = p {p2d = p2d p!>"vw" * listTensor [3,-3] [-a,0,1,0,a,b,0,0,1] !"vw"}
    where a = 2/szx
          b = -szy/szx


signal = mean . map (sqrt . vectorMin . eigenvaluesSH' . snd . meanCov . asMatrix . (~>"nv")) .  flip parts "c" . inhomogT "v" . p2d

signalNoise p = (quality p / signal p)


randomSol seed p = p {p3d = p3d q, cam = cam q} where
    q = mkVProb stdprob {numPoints = n, numCams = c} seed
    n = size "n" (p2d p)
    c = size "c" (p2d p)

trials f n exper = map f . transpose <$> replicateM n exper

quart l = (f a , f b, f c) where
    f = (s!!)
    s = sort l
    n = fromIntegral (length l)
    [a,b,c] = map (round . (*(n-1))) [0.25,0.5,0.75]

-- shQuart = f where
--     --f (a,b,c) = printf "%.2f +- %.2f : %9.5f : %9.5f : %9.5f " b ((c-a)/2) a b c :: String
--       f (a,b,c) = printf "%9.5f : %9.5f : %9.5f : " a b c :: String



shL xs = concatMap (printf "%.5f ") xs


randomPermutation seed xs = map snd $ sortBy (compare `on` fst) $ flip zip xs $ randomRs (0,1::Double)(mkStdGen seed) 

okRangeCoord ((x1,x2),(y1,y2)) = maximum (map abs [x1,x2,y1,y2]) < 1


relocate p = p { p3d = (p3d p * hi)!>"yx", cam = (cam p * h) !> "yx"} where
    hi = applyAsMatrix inv h !"yx"
    h = listTensor [4,-4] [s,0,0,x,
                           0,s,0,y,
                           0,0,s,z,
                           0,0,0, 1] !"xy"
    cens = cameraCenters p
    pts = ihPoints3D p
    (m,c) = meanCov . fromRows $ (concat $ replicate (length cens) pts) ++ (concat $ replicate (length pts) cens)
    [x,y,z] = toList m
    s = sqrt $ vectorMax $ eigenvaluesSH' c 


------------------------------------------------------------------------------------

dino = --(normalizeCoords (640,640).fst) `fmap` 
       loadTracks' "viff.xy"
-- 
-- experdino = do
--     x <- dino
--     let r = randomSol 666 x
--     print $ quality r

----------------------------------------------------------------------------


shSG (a,b) = printf "E2 = %.8f, sigma = %.8f\n" a b


checkNewEpi = do
    -- writeFile "matrixlog.hs" "import Numeric.LinearAlgebra\n\n"
    ---s <- loadSVP "9pts.txt" "9cams.txt" "calib.txt"
    --links <- loadLinks "initiallinks.txt"
    --print links
    s <- loadSVP "54pts.txt" "54cams.txt" "calib.txt"
    --print (v_of_p s 100)
    shSG $ sGError $ s

 --   print $ quality $ mySparseLevmar prob probk0s 10

 --   shSG $ sGError $ mySparseBundle sprob 10

    error "ok"
    shSG $ sGError $ recompPts s
    shSG $ sGError $ recompCams $ s
    shSG $ sGError $ recompCams . recompPts $ s
    shSG $ sGError $ recompCams . recompPts . recompCams . recompPts $ s
    --let tp = tensorProblem s
    --shProb' "orig" (relocate . tensorProblemK $ s)
    shSG $ sGError $ myepi3 s [] 10
--    print $ sGError (myepi3 s [] 0)
--    print $ sGError (myepi3 s links 10)
--    let tq = myepi2 
--     let (model, vsol, newSol, fcost, obs) = prepareEpipolar k s cams
--     print $ fcost vsol
--     print $ sGError (lObs s) k pts cams
--     let sol = myepi3 k s cams 2
-- 
--     print $ sGError (lObs s) k pts sol
--     error "kk"
--     let tp = tensorProblem (pts,cams,k,s)
--         (prejaco, vsol2,_,_) = studyEpipolar2 (repeat (Just k)) tp
--     print $ pnorm PNorm2 $ (toDense $ fst $ prejaco vsol2) - (toDense $ fst $ model vsol2)
--     let sol' = myepi2 tp (repeat (Just k))  1
--     print $ sGError (lObs s) k pts (getCams  sol')
--     print $ head obs


main = do
    args <- getArgs
    case args of
        [fp,fc,fk] -> do
            s <- loadSVP fp fc fk
            putStr "Initial solution: "
            time $  shSG (sGError s)
--            putStr "\nZero iterations: "
--            time $ shSG $ sGError $ myepi3 s [] 0
--            putStr "\nOnly Points: "
--            time $ shSG $ sGError $ onlyPoints s 5
--            putStr "\nOnly Cams: "
--            time $ shSG $ sGError $ onlyCams s 5
--            putStr "\nAlternation: "
--            time $ shSG $ sGError $ alter 8 s
            putStr "\nFull graph: "
            time $ shSG $ sGError $ myepi3 s [] 10
--            putStr "\nFull graph + nonlin pts: "
--            time $ shSG $ sGError $ onlyPoints (myepi3 s [] 10) 5
--            putStr "\nmy SBA: "
--            time $ shSG $ sGError $ mySparseBundle s 10
--         [fp,fc,fk,fl] -> do
--             links <- loadLinks fl
--             s <- loadSVP fp fc fk
--             putStr "Initial solution: "
--             time $  shSG (sGError s)
--             putStr "Selected links: "
--             time $ shSG $ sGError $ myepi3 s links 10


alter k = (!!k) . iterate f
    where f s = onlyCams (onlyPoints s 1) 1
