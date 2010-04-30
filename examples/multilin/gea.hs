-- {-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances #-}

import Numeric.LinearAlgebra.Exterior
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra hiding ((.*), scalar)
import Numeric.LinearAlgebra.Array.Util as Array
-- import Numeric.LinearAlgebra.Array.Decomposition
import Numeric.LinearAlgebra.Array.Solve
import Graphics.Plot
import System.Random
-- import Debug.Trace
import Vision hiding (debug)
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
--import Epipolar
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

n = 200
m = 10

pixNoise = 2
pixFactor = 640/2
sigma = pixNoise / pixFactor
inPix d v = printf ("%."++show (d::Int)++"f pixels") (v * pixFactor)
seeds = [5555,7777,9999,2222]

prob = genProb n m sigma seeds

-- \perform{shProb "cameras" prob}

-- \perform{drawPoints [] (toLists $ asMatrix $ head $ flip parts "c" $ inhomogT "v" $ p2d prob)}

simil3D a b = printf "%.1f%%\n" $ 100 * objectQuality a b


shl l = concat $ intersperse ", " $ map (printf "%.1g") $ l


disp = putStr . dispf 5

-----------------------------------------------------------------




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



------------------------------------------------------------------------------------



-- dino selcams = do
--     s <- loadTracks' "viff.xy"
--     seed <- randomIO
--     let tr = tensorSelect (640,480) selcams s
--     --print (length selpts)
--     let r = relocate $ flipDir $ autoCalibrate Nothing $ fourViews seed $ p2d tr
--     shProb' "kk" r
--     error "ok"
--     let icams = initCams4 seed (650,480) Nothing s
--     mapM_ (disp.normat) (icams)
--     print $ map rank icams
--     drawCameras "kk" icams []

sprob = sparseFromTensor prob

----------------------------------------------------------------------------


shSG (a,b) = printf "E2 = %.8f, sigma = %.8f\n" a b




-- fixCam s = s' { sCam = map (snd.sepCam) (sCam s') }
--     where s' = recompCams s

{-
-- breaking tracks
main2 = do
    args <- getArgs
    case args of
        [fp,fc,fk] -> do
            s <- loadSVP fp fc fk
            putStr "Initial solution: "
            time $  shSG (sGError s)
            putStr "\nFull graph: "
            let sgea =  myepi3 s 10
            time $ shSG $ sGError sgea
            sm <- loadSVP2 (breakTrack 5) fp fc fk
            putStr "Initial solution Break: "
            time $  shSG (sGError sm)
            putStr "\nFull graph Break: "
            time $ shSG $ sGError $ myepi3 sm 10
            print $ (snP s, snP sm)
            -- print $ analyze s
            print $ longis s
            print $ longis sm
--            putStr "\nZero iterations: "
--            time $ shSG $ sGError $ myepi3 s [] 0
--            putStr "\nOnly Points: "
--            time $ shSG $ sGError $ onlyPoints s 5
--            putStr "\nOnly Cams: "
--            time $ shSG $ sGError $ onlyCams s 5
--            putStr "\nAlternation: "
--            time $ shSG $ sGError $ alter 8 s
--            putStr "\nFull graph: "
--            time $ shSG $ sGError $ myepi3 s [] 10
--            putStr "\nFull graph: "
--            time $ shSG $ sGError $ myepi4 s 10


--            putStr "\nFull graph + nonlin pts: "
--            time $ shSG $ sGError $ onlyPoints (myepi3 s [] 10) 5
            putStr "\nmy SBA: "
            let sbun = mySparseBundle 0.001 s 10
            time $ shSG $ sGError $ sbun
            let ss1 = singularValues (epiHessian sgea)
                ss2 = singularValues (hessianCamsBundle 0 sbun)
                ss0 = linspace (dim ss1) (1, fromIntegral (dim ss1))
            mplot [ss0, logBase 10 (1E-10 + ss1/LA.scalar (ss1@>0)), logBase 10 (1E-10 + ss2/LA.scalar (ss2@>0))]
            putStr "\nmy SBA Break: "
            time $ shSG $ sGError $ mySparseBundle 0 sm 10

--         [fp,fc,fk,fl] -> do
--             links <- loadLinks fl
--             s <- loadSVP fp fc fk
--             putStr "Initial solution: "
--             time $  shSG (sGError s)
--             putStr "Selected links: "
--             time $ shSG $ sGError $ myepi3 s links 10

-}


checkNewEpi selcams = do
    -- writeFile "matrixlog.hs" "import Numeric.LinearAlgebra\n\n"
    ---s <- loadSVP "9pts.txt" "9cams.txt" "calib.txt"
    --links <- loadLinks "initiallinks.txt"
    --print links
    seed <- randomIO
    --s <- loadSVP "54pts.txt" "54cams.txt" "calib.txt"
    s <- loadSVP "9pts.txt" "9cams.txt" "calib.txt"
    print (rangeCoords s)
    shSG $ sGError $ s
    let icams = initCams4 seed (640,480) Nothing s
    print $ map rank icams
    drawCameras "kk" icams []
--    drawCameras "qq" (sCam s) []
    let fs = map (getFK.fst.sepCam) icams
    print fs
    let f = median (take 5 fs)
        k' = knor (640,480) <> kgen f
        estKs = replicate (snC s) k'
        estCs = map (snd . sepCam . (k' <>) . snd . sepCam) icams
        sa = recompPts s { sKal = estKs, sCam = estCs }
    disp k'
    shSG $ sGError $ sa
--    error "ok"
    shSG (sGError $ myepi3 s 10)
    shSG (sGError $ myepi3 sa 20)
    shSG (sGError $ mySparseBundle 0.01 s 10)
    shSG (sGError $ mySparseBundle 0.01 sa 10)

    error "ok"
    let tr = tensorSelect (640,480) selcams s

--    print (length selpts)
    let r = relocate $ flipDir $ autoCalibrate Nothing $ fourViews seed $ p2d tr
    shProb "kk" r
    sh $ dummyAt 1 $ cam r ~> "cvx"
    mapM_ (disp.normat) (getCams r)
--    let tr3 = p2d $ tensorSelect (640,480) (init selcams) s
    disp $ extractFirst (p2d tr) (tail $ getCams r)
    error "ok"
    sh $ kal $ cam $ r
    let k' = knor (640,480) <> diag (3|>[3,3,1])
    disp k'
    let estKs = replicate (snC s) k'
        estCs = map (snd . sepCam . (k' <>)) (sCam s)
        sa = s { sKal = estKs, sCam = estCs }
    shSG (sGError $ myepi3 s 10)
    shSG (sGError $ myepi3 sa 10)
    shSG (sGError $ mySparseBundle 0.010 sa 10)
    return r

--param = defaultParameters {post = eqnorm}
--threeViews = multiView 3 param

 --   print $ quality $ mySparseLevmar prob probk0s 10

 --   shSG $ sGError $ mySparseBundle sprob 10


{-    error "ok"
    shSG $ sGError $ recompPts s
    shSG $ sGError $ recompCams $ s
    shSG $ sGError $ recompCams . recompPts $ s
    shSG $ sGError $ recompCams . recompPts . recompCams . recompPts $ s
    --let tp = tensorProblem s
    --shProb' "orig" (relocate . tensorProblemK $ s)
    shSG $ sGError $ myepi3 s 10-}
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


-- optimization of a problem from the given intialization
main3 = do
    args <- getArgs
    case args of
        pname:_ -> do
            let [fp,fc,fk] = map f ["pts.txt","cams.txt","calib.txt"]
                    where f n = "../../data/tracks/"++pname++"/"++n
            s <- loadSVP fp fc fk
            putStr "Initial solution: "
            time $ shSG (sGError s)
            shProbS "initial" s
            putStr "\nFull graph: "
            let sgea =  myepi3 s 10
                sel = [0..snC s-1]
                sgea2 = gea sel sel s
            time $ shSG $ sGError sgea
            time $ shSG $ sGError sgea2
            shProbS "GEA refinement" sgea



-- try to initialize the problem using the known calib
main = do
    args <- getArgs
    case args of
        [pname] -> do
            let [fp,fc,fk] = map f ["pts.txt","cams.txt","calib.txt"]
                    where f n = "../../data/tracks/"++pname++"/"++n
            seed <- randomIO
            s <- loadSVP fp fc fk
            putStr "Initial solution: "
            time $ shSG (sGError s)
            --print (rangeCoords s)
            let t = initStart seed [0..3] s
            shProbS "bootstrap" t
            shSG $ sGError t
{-            shProb' "" t
            print $ quality t
            print $ autoMetric t
            let fs = map (getFK.fst.sepCam) (getCams t)
            print fs
            let sl = sliceProblem [0,1,2,3] s
            print (snC sl, snP sl)
            putStr "slice     " ; shSG (sGError sl)
            putStr "slice opt " ; shSG (sGError $ myepi3 sl 10)
            let sr = recompPts sl { sCam = map (snd.sepCam) (getCams t) }
            putStr "slice init " ; shSG (sGError sr)
            putStr "slice iopt "; shSG (sGError $ myepi3 sr 10)-}
            error "ok"
--             let (icams,ipts) = initCams43 seed sz mbf s
--             print $ map rank icams
--             drawCameras "kk" (take 4 icams) ipts
--             let fs = map (getFK.fst.sepCam) icams
--             print fs
--             let f = median fs
--                 k' = knor sz <> kgen f
--                 estKs = replicate (snC s) k'
--                 estCs = map (snd . sepCam . (k' <>) . snd . sepCam) icams
--                 sa = recompPts s { sKal = estKs, sCam = estCs }
--             disp k'
--             shSG $ sGError $ sa
--             shProbS sa
--             let sgea = myepi3 sa 10
--             shSG (sGError $ sgea)
--             shProbS sgea
--    error "ok"
--    shSG (sGError $ myepi3 s 10)
--    shSG (sGError $ myepi3 sa 20)
            
            
--             shProbS s
--             putStr "\nFull graph: "
--             let sa =  myepi3 s 10
--             time $ shSG $ sGError sgea
--             shProbS sa


checkit = do
    p <- sparseFromTensor `fmap` randomVProb stdprob {numPoints = 100, numCams = 20, pixelNoise = 1, fov = 50*degree, minDist = 2, maxDist = 3}
    let sgea = myepi3 p 10
        sbun = mySparseBundle 0.001 p 10
        sgeabun = onlyPoints 0.001 sgea 10
    print $ sGError sgea
    print $ sGError sgeabun
    print $ sGError sbun
    printf "Object Q: GEAL = %.5f, GEAB = %.5f, SBA = %.5f\n" (objectQualityS p sgea) (objectQualityS p sgeabun) (objectQualityS p sbun)
    printf "Pose   Q: GEA = %.5f, SBA = %.5f\n" (poseQualityS p sgea) (poseQualityS p sbun)



gea used free = fst . debug "" snd . geaG 1 10 0.001 used free


checkSelect = do
    let sg = myepi3 sprob 10
        ss = gea [0..9] [2..9] sprob
    print $ sGError sg
    print $ sGError ss


alter k = (!!k) . iterate f
    where f s = onlyCams 0.001 (onlyPoints 0.001 s 1) 1

-------------------

-- analyze s = sortBy (compare `on` snd) $ map g (epiObs2 s)
--     where g (p,m) = (p,round $ 1E5*rcond m)

--longipairs s = sort (map (rows.snd) $ epiObs2 s)

longis s = take 100 . reverse . sort . map (length . v_of_p s) $  [0 .. snP s -1]

shProbS tit s = shProb tit $ relocate $ (tensorProblemK [0..snP s -1] [0..snC s -1] s)

-------------------------------------------------------------

initCams3 seed sz mbf s = cs where
    cs = map f [0 .. snC s -1]
    t = p2d $ tensorSelect sz [0,1,2] s
    [c0,c1,c2] = getCams $ debug "QT = " quality $ relocate $ flipDir $ autoCalibrate mbf $ fourViews seed $ t
    f 0 = c0
    f 1 = c1
    f 2 = c2
    f k = extractFirst (p2d $ tensorSelect sz [k,k-2,k-1] s) [cs!!(k-2), cs!!(k-1)]

initCams4 seed sz mbf s = cs where
    cs = map f [0 .. snC s -1]
    t = p2d $ tensorSelect sz [0,1,2,3] s
    [c0,c1,c2,c3] = getCams $ debug "QQ = " quality $ relocate $ flipDir $ autoCalibrate mbf $ fourViews seed $ t
    f 0 = c0
    f 1 = c1
    f 2 = c2
    f 3 = c3
    f k = extractFirst (p2d $ tensorSelect sz [k,k-3,k-2,k-1] s) [cs!!(k-3),cs!!(k-2), cs!!(k-1)]

initCams43 seed sz mbf s = (cs,ipts) where
    cs = map f [0 .. snC s -1]
    t = p2d $ tensorSelect sz [0,1,2,3] s
    r = relocate $ flipDir $ autoCalibrate mbf $ fourViews seed $ t
    ipts = map (toList.inHomog) (getP3d r)
    [c0,c1,c2,c3] = getCams $ debug "QQ = " quality $ r
    f 0 = c0
    f 1 = c1
    f 2 = c2
    f 3 = c3
    f k = extractFirst (p2d $ tensorSelect sz [k,k-2,k-1] s) [cs!!(k-2), cs!!(k-1)]

initStart seed selcams s = so where
    t = p2d $ tensorSelectK selcams s
    r = relocate $ flipDir $ autoCalibrate (Just 1) $ fourViews seed $ t
    sl = sliceProblem selcams s
    sr = recompPts sl { sCam = map (snd.sepCam) (getCams r) }
    sel = [0.. length selcams -1]
    so = gea sel sel sr

-- hmm, it is better to select variables and observations
-- points change indices, must rename cameras...
sliceProblem selcams s = r where
    obs1 = [ o | o@((_,v),_) <- lObs s, v `elem` selcams ]
    selpts = map (fst.fst) obs1
    obs2 = zipWith (map . g) [0..] $ groupBy ((==) `on` fst.fst) obs1
        where g n ((_,v),x) = ((n,v),x)
    r = (goSparse obs2) { sPts = selectPos selpts  $ sPts s,
                          sCam = selectPos selcams $ sCam s,
                          sKal = selectPos selcams $ sKal s }

-- | replace elements of xs at given positions by ys
replaceAt pos ys xs = zipWith f [0..] xs where
    g = flip lookup (zip pos ys)
    f k x = case g k of
        Just y -> y
        Nothing -> x
