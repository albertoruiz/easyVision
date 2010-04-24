{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances #-}

import Numeric.LinearAlgebra.Exterior
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra hiding ((.*), scalar)
import Numeric.LinearAlgebra.Array.Util as Array
import Numeric.LinearAlgebra.Array.Decomposition
import Numeric.LinearAlgebra.Array.Solve
import Graphics.Plot
import System.Random
import Debug.Trace
import Vision
import Vision.Multiview
import System
import Data.List
--import qualified Data.Map as Map
import Data.Function(on)
import Control.Monad hiding (join)
import Control.Applicative hiding ((<|>))
import Text.Printf
--import Numeric.GSL.Minimization
--import Numeric.GSL.Differentiation
--import Numeric.GSL.Fitting
--import Bundle hiding (alter)
--import Epipolar
import System.CPUTime
--import System.IO
--import Util.Sparse
--import Data.Maybe
import Foreign(unsafePerformIO)
import System.IO


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


sh = putStr . shN
shN t = formatFixed 2 $ t  -- for use at this document
shp t = putStrLn . formatFixed 5 $ t

-- We generate a pseudorandom testing configuration with EVAL(n) points, EVAL(m) cameras, and a very small amount of gaussian image noise with standard deviation EVAL(inPix 2 sigma). We assume that the image resolution is 640 pixels, although we use normalized image coordinates from $-1$ to $+1$ to improve numerical stability.

n = 15
m = 6

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

check meth p = (q,s) where
   sol = meth (p2d p)
   q = quality sol
   s = simil3D' p sol

checkR mg pg = do
   m <- mg
   p <- pg
   return (check m p)

checksR n mg pg = do
  (qs,ss) <- unzip <$> replicateM n (checkR mg pg)
  putStrLn $ inPix 2 (median qs)
          ++ printf ", %.2f metric quality\n" (median ss)


rep t meth n c noise =  checksR t (meth <$> randomIO) (randomProb n c (noise/pixFactor))


refine p = p { p3d = newpoints, cam = newcams }
    where ([newcams,newpoints], err) = mlSolveP param [1] [cam p, p3d p] (p2d p) "v"


fourViews = multiView 4 param
threeViews = multiView 3 param
twoViews = multiView 2 param

-- from kal . cam
getFs = map ((/2) . sum . toList . subVector 0 2 . takeDiag . asMatrix) . flip parts "c"

bundleE p = bundleT (fixCal . kal . cam $ p) p

fixCal = subindex "c" . map ((!>"1v 2w") . fromMatrix Contra Co . kgen) . getFs

autoMetric p = frobT (k - fixCal k) / fromIntegral (5 * size "c" k)
     where k = kal . cam $ p



co = fromMatrix Contra Co cameraAtOrigin

param = defaultParameters {post = eqnorm}

shl l = concat $ intersperse ", " $ map (printf "%.1g") $ l


disp = putStr . dispf 5

-----------------------------------------------------------------

experB n c noise = do
   -- p <- randomProb n c (noise/pixFactor)
   p <- randomVProb stdprob {numPoints = n, numCams = c, pixelNoise = noise, fov = 50*degree, minDist = 2, maxDist = 3}
   shProb "kkkk" p
   when (not . okRangeCoord . rangecoord $ p) $ putStrLn "Warning: some pixels are out of FOV !!!"
   seed <- randomIO
   let k  = kal . cam $ p
       f  = median . getFs . kal . cam $ p
       b  = bundleT k p
       r  = refine p
       t0 = fourViews seed . p2d $ p
       t  = refine t0
       m0 = autoCalibrate Nothing t0
       mf0 = autoCalibrate (Just f) t0
       m  = autoCalibrate Nothing t
       mf  = autoCalibrate (Just f) t
       bt = bundleE m0
       tt = bundleT k mf
       ran = randomSol seed p
       nai = autoCalibrate (Just f) . refine $ ran
--   putStrLn "    METHOD        Rep Err     Signal     S/N      AutoMet     Met Cons    Pose Cons"
--   putStrLn "-------------     -------     ------    ------    -------     --------    ---------"
--   infosol p "Ground truth" b
   --infosol p "    ALS     " r
   --infosol p "    BUN     " b
--   infosol p "Random sol  " ran
-- infosol p "Naive ALS   " nai
--   infosol p "Raw reconst " m0
--   infosol p "    K       " mf0
--   infosol p "    ALS     " m
--   infosol p "    ALS K   " mf
   --infosol p "    BUN est " bt
--   infosol p "    BUN K   " tt
--   putStrLn ""
   let x = concatMap (infosection p) $ 
            [p, b, nai, m, mf, tt]
   putStrLn $ concatMap (printf "%.1f ") x
   return x

infosection p s = map ((100*).($s)) [signalNoise, autoMetric, simil3D' p, poseQuality p]



par k = pl!!(k-1) where
  pl = [meth++"_"++param | meth <-  ["true", "truebundle", "ALSnaive", "autocalFree", "autocalFixed", "AFbundle"] 
                    ,  param <- ["signalNoise", "autoMetric","ObjectQ","PoseQ"]]


labels = concatMap f $ 
            ["true", "true bundle", "naive", "autocal", "K", "bundle"]
   where f s = ["signalNoise "++s++replicate 20 '-', "autoMetric","Object Q","Pose Q"]



infosol p msg s = printf (msg ++ "%10.2f pix %8.1f %8.1f %10.1f %10.1f%%  %10.1f%%\n") q sg sn am m pose
    where q = quality s * pixFactor
          m = 100 * simil3D' p s
          pose = 100 * poseQuality p s
          sg = signal s * pixFactor
          sn = 100*signalNoise s
          am = 100* autoMetric s




rangecoord p = ((vectorMin x, vectorMax x),(vectorMin y, vectorMax y)) where
    [x,y] = toRows $  fibers "v" (inhomogT "v" $ p2d p)

normalizeCoords (szx,szy) p = p {p2d = p2d p!>"vw" * listTensor [3,-3] [-a,0,1,0,a,b,0,0,1] !"vw"}
    where a = 2/szx
          b = -szy/szx


analyzeFile ps sz filename = do
   p <- normalizeCoords sz <$> vprobFromFiles (filename++".cam.txt") (filename++".img.txt") (filename++".pts.txt")
   let nv = size "c" (p2d p)
   printf "Points=%d, Views=%d\n" (size "n" (p2d p)) nv 
   let ((x1,x2),(y1,y2)) = rangecoord p in printf "Normalized range: (%.2f,%.2f) (%.2f,%.2f)\n" x1 x2 y1 y2
   seed <- randomIO
   let t0 = case ps of
             [] -> fourViews seed . (randomPermutation seed `onIndex` "c") . p2d $ p
             ps -> fourViews seed . ((\x-> map (x!!) (ps++([0..nv-1]\\ps))) `onIndex` "c") . p2d $ p
       t  = refine t0
       -- m0 = autoCalibrate Nothing t0
       mr = autoCalibrate Nothing t
       m  = relocate . flipDir $ mr
       -- ran = randomSol seed p
       -- nai = autoCalibrate Nothing . refine $ ran
   --info "Raw reconst." m0
   ok <- info "" m
   --info "    naive   " nai
   --print $ getFs . kal . cam $ m0
   --print $ getFs . kal . cam $ m
   --when ok $ shProb filename m
   when ok $ shProb' filename m
   let names@[fc,fi,fp,fk] = map ((filename++"-result")++) $ words ".cam .img .pts .kal"
   --print names
   when ok $ vprobToFiles fc fi fp fk (kal $ cam m) m
   when ok $ system("rm "++fi) >> return ()
   --print $ meanCov $ fromRows $ ihPoints3D $ mr
   --print $ meanCov $ fromRows $ ihPoints3D $ m
 where
    info msg s = do printf (msg ++ "Geometric error=%.2f pix, Signal strength=%.1f pix, NSR=%.1f%%, AutoCalQ=%.1f%%\n") q sg sn am
                    let ok = sn<2 && am<1
                    when (not ok) $ putStrLn "warning: poor convergence, please run again."
                    return ok
        where q = quality s * (fst sz/2)
              sg = signal s * (fst sz/2)
              sn = 100* signalNoise s
              am = 100* autoMetric s

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

shQuart = f where
    --f (a,b,c) = printf "%.2f +- %.2f : %9.5f : %9.5f : %9.5f " b ((c-a)/2) a b c :: String
      f (a,b,c) = printf "%9.5f : %9.5f : %9.5f : " a b c :: String


experimentQ t n c noise = do
     printf "points = %d, cameras = %d, sigma = %.2f\n" n c noise
     mapM_ putStrLn.zipWith (flip(++)) (zipWith g [1::Int ..] labels) =<< trials (shQuart.quart) t (experB n c noise)
  where g k l = "  "++show k++"  "++l


experimentL t n c noise = do
    printf "points = %d, cameras = %d, sigma = %.2f\n" n c noise
    mapM_ putStrLn.zipWith (flip(++)) (zipWith g [1::Int ..] labels) =<< trials (shL) t (experB n c noise)
 where g k l = " :  "++show k++"  "++l

shL xs = concatMap (printf "%.5f ") xs



randomPermutation seed xs = map snd $ sortBy (compare `on` fst) $ flip zip xs $ randomRs (0,1::Double)(mkStdGen seed) 

okRangeCoord ((x1,x2),(y1,y2)) = maximum (map abs [x1,x2,y1,y2]) < 1



experimentR file t n c noise = do
     let h = printf "# points = %d, cameras = %d, sigma = %.2f\n" n c noise
     -- mapM_ putStrLn.zipWith (flip(++)) (zipWith g [1::Int ..] labels) 
     r <- unlines . lab . map ((\l->" <- c("++l++");"). unwords . intersperse "," . words) <$> trials (shL) t (experB n c noise) 
     writeFile file $ h ++ r
  where g k l = par k ++l
        lab x = zipWith g [1::Int ..] x


experimentQMOnly t n c noise = do
     hSetBuffering stdout NoBuffering
     let h = printf "Trials = %d\nPoints = %d, Cameras = %d, sigma = %.2f\n\n" t n c noise
         h2 = "    Q25        Median       Q75     Parameter(%)   Method\n\n"
     r <- unlines . spe 3 . lab  <$> trials (shQuart.quart) t (experMultilinOnly n c noise)
     putStrLn $ replicate 70 ' ' ++ "\r" ++ h ++ h2 ++ r
  where -- g k l = "  "++show k++"  "++l
        g k l = l ++ par2 k
        lab x = zipWith g [1::Int ..] x

experMultilinOnly n c noise = do
   -- p <- randomProb n c (noise/pixFactor)
   p <- randomVProb stdprob {numPoints = n, numCams = c, pixelNoise = noise, fov = 50*degree, minDist = 2, maxDist = 3}
   shProb "kkkk" p
   when (not . okRangeCoord . rangecoord $ p) $ putStrLn "Warning: some image points are out of FOV !!!"
   seed <- randomIO
   let k  = kal . cam $ p
       k0s = map (Just . kgen) . getFs . kal . cam $ p
       f  = median . getFs . kal . cam $ p
       r  = refine p
       t0 = fourViews seed . p2d $ p
       t  = refine t0
       m0 = autoCalibrate Nothing t0
       mf0 = autoCalibrate (Just f) t0
       m  = autoCalibrate Nothing t
       mf  = autoCalibrate (Just f) t
       ran = randomSol seed p
       nai = autoCalibrate (Just f) . refine $ ran
--       gslbun = gsllevmar mf0 k0s 20
--       mybun = mylevmar mf0 k0s 20
--       mysbun = mySparseLevmar mf0 k0s 20
       sba = bundleT (kal (cam p)) mf0
   let x = concatMap (infosection2 p) $ 
            [nai, m, mf, --mysbun, mybun, gslbun, 
                        sba, p]
       g x = printf "%.2f " x
   putStr $ (++"\r") $ concatMap g x
   --hFlush stdout
   return x

infosection2 p s = map ((100*).($s)) [signalNoise, simil3D' p, poseQuality p]


par2 k = pl!!(k-1) where
  pl = [param ++ "  " ++ meth | meth <-  ["ALS naive, known K  ", "multilin, autocal", "multilin, known K",
  --"mysbun", "mybun", "gslbun",
   "sba", "true solution"]
                          ,  param <- ["Noise/Signal ", "ObjectQuality","PoseQuality  "]]

spe k = concat . intersperse [[]]. splitEvery 3

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


gnuplotWin :: String -> String -> [([[Double]], String)] -> IO ()
gnuplotWin title command ds = gnuplot (command ++" "++ draw) where
    (dats,defs) = unzip ds
    draw = concat (intersperse ", " (map ("\"-\" "++) defs)) ++ "\n" ++
           concatMap pr dats

    pr = (++"e\n") . unlines . map (unwords . (map show))

    gnuplot cmd = do
        writeFile "gnuplotcommand" cmd
        _ <- system "gnuplot -persist gnuplotcommand"
        _ <- system "rm gnuplotcommand"
        return ()


shcam :: Matrix Double -> [[Double]]
shcam p = c where
   (h,f) = toCameraSystem p
   c = ht (h <> diag (fromList [1,1,1,15])) (cameraOutline' f)

drawCameras :: String -> [Matrix Double] -> [[Double]] -> IO ()
drawCameras tit ms pts = do
  let cmd = map (f.shcam) ms
      f c = (c,"notitle 'c1' with lines 1")

  gnuplotWin tit
         (  "set view 72,200; "
         ++ "set pointsize 0.1;"
         ++ "set xlabel 'x'; set ylabel 'y'; set zlabel 'z';"
         ++ "set xrange [-2:2]; set yrange [-2:2]; set zrange [-2:2];"
         ++ "set size ratio 1;"
         ++ "set ticslevel 0;"
         ++ "set notics;"
         ++ "splot ")
         (cmd ++ [(pts,"notitle 'v' with points 7")])

shProb' :: String -> VProb -> IO ()
shProb' tit p = drawCameras tit
    (map asMatrix $ parts (cam p) "c")
    (toLists $ asMatrix $ inhomogT "x" $ p3d p)

cameraOutline' f =  [0::Double,0,0] : drop 5 (cameraOutline f)

flipDir p = {- debug "dir=" (const d) -} p { p3d = (p3d p * hi)!>"yx", cam = (cam p * h) !> "yx"} where
    hi = applyAsMatrix inv h !"yx"
    h = listTensor [4,-4] [1,0,0,0,
                           0,1,0,0,
                           0,0,1,0,
                           0,0,0,d] !"xy"
    c1 = asMatrix $ head $ parts (cam p) "c"
    p1 = head $ ihPoints3D p
    d = signum $ depthOfPoint (toList p1) c1


main = do
   args <- getArgs
   case args of
       [t,n,c,sigma] -> experimentQMOnly (read t) (read n) (read c) (read sigma)
       [szx,szy,filename] -> analyzeFile [] (read szx, read szy) filename
       [szx,szy,filename,v1,v2,v3,v4] -> analyzeFile (nub[read v1,read v2, read v3, read v4]) (read szx, read szy) filename

------------------------------------------------------------------------------------

bundleT k p = unsafePerformIO $ do
   (name,h) <- openTempFile "/tmp" "bundle"
   let names@[fc,fi,fp,fk,rc,rp] = map (name++) $ words ".cam .img .pts .kal .ref.cam .ref.pts"
   --print names
   vprobToFiles fc fi fp fk k p
   system $ "demo-sba "++unwords [fc,fp,fi,fk,rc,rp]++"> /dev/null 2> /dev/null"
   r <- vprobFromFiles rc fi rp
   hClose h
   system $ "rm "++unwords (name:names)
   return r
