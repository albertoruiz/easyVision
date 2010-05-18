{-# LANGUAGE RecordWildCards #-}

import Numeric.LinearAlgebra.Exterior
import qualified Numeric.LinearAlgebra as LA
import Numeric.LinearAlgebra hiding ((.*), scalar)
import Numeric.LinearAlgebra.Array.Util as Array
-- import Numeric.LinearAlgebra.Array.Decomposition
import Numeric.LinearAlgebra.Array.Solve
import Graphics.Plot
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
--import Epipolar
import System.CPUTime
-- import System.IO
-- import Util.Sparse
import Data.Maybe
import Util.Misc(mean,median,debug,arrayOf,myintersect,norm, replaceAt)

import qualified Data.Array as A
--import qualified Data.Vector as V



-- import Debug.Trace
-- 
-- debug msg f x = trace (msg ++ show (f x)) x
-- 
-- debugT t = trace (formatFixed 5 t) t

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

n = 100
m = 20

pixNoise = 1
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





-- optimization of a problem from the given intialization
main3 = do
    args <- getArgs
    case args of
        pname:_ -> do
            let [fp,fc,fk] = map f ["pts.txt","cams.txt","calib.txt"]
                    where f n = "../../data/tracks/"++pname++"/"++n
            s <- loadSVP fp fc fk
            infoSProb s
            putStr "Initial solution: "
            time $ shSG (sGError s)
            shProbS "initial" s
            putStr "\nFull graph: "
            --let sbun = mySparseBundle 1 s 10
            --printf "SBA %.3f\n" $ (snd $ sGError sbun)
--            let sbun = mySparseBundle s 10
--            time $ printf "SBA %.3f\n" $ (snd $ sGError sbun)
            let sgea = geaFull s
            time $ printf "GEA %.3f\n" $ (snd $ sGError sgea)
            shProbS "GEA refinement" sgea
--            let sgea = geaFull $ mySparseBundle 100 (geaFull s) 10

--            let sig = snd $ sGError sgea
--            printf "euc %.3f\n" $ sig
--            printf "epi %.3f (%0.3f)\n" (snd $ sEError sgea) (sig*0.7)
--            printf "emp %.3f\n" $ snd $ sEEError sgea
--            printf "alg %.3f\n" $ snd $ sEAError sgea



-- try to initialize the problem using the known calib
main = do
    args <- getArgs
    case args of
        [pname, used, free] -> do
            let [fp,fc,fk] = map f ["pts.txt","cams.txt","calib.txt"]
                    where f n = "../../data/tracks/"++pname++"/"++n
--            seed <- randomIO
            s <- loadSVP fp fc fk
            let s = someHelix
            putStrLn $ "Problem: "++pname
            infoSProb s
            printf "Initial sigma = %.2f\n" (snd $ sGError s)
            --print (rangeCoords s)
            let t = bootstrapEssential (read used) (read free) s
            printf "sigma boot = %.2f\n" (snd $ sGError t)
            shProbS "bootstrap" t
            let r = geaFull t
            printf "sigma auto = %.2f\n" (snd $ sGError r)
            shProbS "GEA" r
            let sgea = geaFull s
            printf "sigma ini = %.2f\n" (snd $ sGError sgea)
            let sbun = mySparseBundle s 10
            printf "sigma bun = %.4f\n" (snd $ sGError sbun)
--             let sgeabun = geaFull sbun
--             printf "sigma bun = %.4f\n" (snd $ sGError sgeabun)


checkit = do
    p <- sparseFromTensor `fmap` randomVProb stdprob {numPoints = 100, numCams = 20, pixelNoise = 1, fov = 50*degree, minDist = 2, maxDist = 3}
    let sgea = geaFull p
        sbun = mySparseBundle p 10
        sgeabun = onlyPoints 0.001 sgea 10
    print $ sGError sgea
    print $ sGError sgeabun
    print $ sGError sbun
    printf "Object Q: GEAL = %.5f, GEAB = %.5f, SBA = %.5f\n" (objectQualityS p sgea) (objectQualityS p sgeabun) (objectQualityS p sbun)
    printf "Pose   Q: GEA = %.5f, SBA = %.5f\n" (poseQualityS p sgea) (poseQualityS p sbun)



gea used free = fst . debug "" snd . geaG 1 10 0.001 used free
geaFull s = recompPts $ gea sel sel s where sel = [0.. snC s -1]


-- checkSelect = do
--     let sg = myepi3 sprob 10
--         ss = recompPts $ gea [0..9] [2..9] sprob
--     print $ sGError sg
--     print $ sGError ss


checkBoot used free = do
    p <- sparseFromTensor `fmap` randomVProb stdprob {numPoints = 100, numCams = 20, pixelNoise = 1, fov = 50*degree, minDist = 2, maxDist = 3}
    let ini = bootstrapTrifocal used free p
        sol = geaFull ini
    print $ sGError p
    print $ sGError ini
    print $ sGError sol




-------------------

-- analyze s = sortBy (compare `on` snd) $ map g (epiObs2 s)
--     where g (p,m) = (p,round $ 1E5*rcond m)

--longipairs s = sort (map (rows.snd) $ epiObs2 s)

longis s = map (length . v_of_p s) $  [0 .. snP s -1]

viewed s = map (length . p_of_v s) $  [0 .. snC s -1]

shProbS tit s = shProb tit $ relocate $ (tensorProblemK [0..snP s -1] [0..snC s -1] s)

-- in raw coordinates
signalLevelS s = mean . map (sqrt . vectorMin . eigenvaluesSH' . snd . meanCov . fromLists . visible s) $ [0..snC s -1]

visible s v = map (init . (flip $ curry (aObs s A.!)) v) (p_of_v s v)

epiQuality m = s 8 / s 9 -- (s 8 - s 9) / (s 7 - s 8)
    where s = (singularValues m @>) . pred

epiRank r m = s r ^ 2 / s (r-1) / s (r+1) 
    where s = (singularValues m @>) . pred

epiCond r m = s 8 / s 1
    where s = (singularValues m @>) . pred

-- must reuse m
homographicQ s i j = if length com < 9 then 0 else err where
    h = estimateHomography pjs pis
    com = commonPoints s i j
    ako' s = toList . inHomog . ako s
    f k = (ako' s (k,i), ako' s (k, j))
    (pis,pjs) = unzip $ map f com
    pjs' = ht h pis
    err = sqrt $ (norm $ flatten (fromLists pjs' - fromLists pjs)) ^ 2 / fromIntegral (length com)

-------------------------------------------------------------

---------------------------------------------------------------------------------------

initStart seed selcams s = (result,r) where
    t = p2d $ tensorSelectK selcams s
    r = relocate $ flipDir $ autoCalibrate (Just 1) $ fourViews seed $ t
    so = s {sCam = replaceAt selcams (getCams r) (sCam s)}
    result = gea selcams selcams so

improveStep s k = sr where
 --   guess = extractFirst (p2d $ tensorSelectK [k,k-2,k-1] s) [sCam s!!(k-2), sCam s!!(k-1)]
    guess = extractFirst (p2d $ tensorSelectK [k,k-3,k-2,k-1] s) [sCam s!! (k-3), sCam s!!(k-2), sCam s!!(k-1)]
 --   guess = unifmov (sCam s!!(k-2)) (sCam s!!(k-1))
    st = s {sCam = replaceAt [k] [guess] (sCam s)}
    r = gea [0.. k-1] [k] st
    sr = gea [0..k] [0..k] r

bootstrap s = foldl' improveStep so [4 .. snC s -1]
    where so = fst $ initStart 777 [0..3] s

----------------------------------------------------------------------------------------

unifmov m1 m2 = c3 where
    (_,r1,c1) = factorizeCamera m1
    (_,r2,c2) = factorizeCamera m2
    r3 = r2 <> trans r1 <> r2
    c3 = fromBlocks [[r3 , asColumn (-r3 <> (c2-c1))]]

----------------------------------------------------------------------------------------

checkVisibPts = do
    args <- getArgs
    case args of
        [pname] -> do
            let [fp,fc,fk] = map f ["pts.txt","cams.txt","calib.txt"]
                    where f n = "../../data/tracks/"++pname++"/"++n
--            seed <- randomIO
            s <- loadSVP fp fc fk
            putStrLn $ "Problem: "++pname
            infoSProb s
            let r = recompCamsSel s [0..4] [5]
            print $ sGError r




empiricRcond = 1/0.65

infoSProb = infoSProb' 1

infoSProb' pixFact s = do
    printf "cameras      = %d\n" (snC s)
    printf "tracks       = %d\n" (snP s)
    printf "views        = %d\n" (length (lObs s))
    shDisI "track length = " (quart . longis $ s)
    shDisI "points/view  = " (quart . viewed $ s)
    let obs = epiObs s
        fullobs = filter ((>8).rows.snd) obs
    printf "pairs        = %d tot, %d ok\n" (length obs) (length fullobs )
    let (noib,noise,noia) = quart $ map (rcond.snd) fullobs
        sl  = signalLevelS s
        f = median $ map getFK (sKal s)
    shDist "rcond        = " (-logBase 10 noib,-logBase 10 noise,-logBase 10 noia)
    printf "signal       = %.1f\n" sl
    printf "typ f        = %.1f\n" f
    printf "est sigma    = %.1f\n" (noise*f*empiricRcond*pixFact)
    printf "est N/R      = %.1f %%\n" (100*noise*f*empiricRcond/sl)
    putStrLn "------------------------"
--    printf "geom dist    = %.1f\n" (snd (sGError s)*pixFact)

infoCosts' pixFact s = do
    printf "geom dist    = %.1f\n" (snd (sGError s)*pixFact)
    printf "epi dist     = %.1f\n" (snd (sEError s)*pixFact)
    printf "empiric epi  = %.1f\n" (snd (sEEError s)*pixFact)
    printf "algeb epi    = %.1f\n" (snd (sEAError s)*pixFact)



--    putStr. dispf 1 . (1000*) $ buildMatrix (snC s) (snC s) (uncurry $ flip $ homographicQ s)
--    let chainH = [((j-1,j), (1000 * homographicQ s (j-1) j , length (commonPoints s (j-1) j)))| j <- [1 .. snC s -1]]
--    mapM_ shQLink chainH
--     putStrLn "------------------------"
--     let chain9c = filter (\((i,j),_) -> i+1 == j) $ map (\((i,j),m) -> ((i,j), (recip $ epiCond 9 m, length (commonPoints s i j)))) fullobs
--     mapM_ shQLink chain9c
--     putStrLn "------------------------"
--     let chain8c = filter (\((i,j),_) -> i+1 == j) $ map (\((i,j),m) -> ((i,j), (recip $ epiCond 8 m, length (commonPoints s i j)))) fullobs
--     mapM_ shQLink chain8c
--     putStrLn "------------------------"
--     let chain8 = filter (\((i,j),_) -> i+1 == j) $ map (\((i,j),m) -> ((i,j), (recip $ epiCond 7 m, length (commonPoints s i j)))) fullobs
--     mapM_ shQLink chain8
--     putStrLn "------------------------"
--     let chain6 = filter (\((i,j),_) -> i+1 == j) $ map (\((i,j),m) -> ((i,j), (recip $ epiCond 6 m, length (commonPoints s i j)))) fullobs
--     mapM_ shQLink chain6
--     putStrLn "------------------------"


--    let goodobs = filter g fullobs where g((i,j),_) = length (commonPoints s i j) >= (lQ . quart . viewed $ s) `div` 2
--        eQq@(eQb,eQ,eQh) = quart $ map (epiQuality.snd) goodobs
--    printf "good links   = %d\n" (length goodobs)
--    shDist "epipolar Q   = " eQq
--    
--    let prechain = map (\((i,j),m) -> ((i,j), (round $ epiQuality m, length (commonPoints s i j)))) goodobs
--    print $ sortBy (compare `on` (negate.fst.snd)) prechain
--    mapM_ print $ sortLinks s

shDist name (a,b,c) = printf (name ++ "%.1f (%.1f, %.1f)\n") b a c
shDisI name (a,b,c) = printf (name ++ "%d (%d, %d)\n") b a c

shQLink ((i,j),(q,n)) = printf "c %3d: %.1f (%d)\n" j q n

checkInfo = do
    s <- sparseFromTensor `fmap` randomVProb stdprob {numPoints = 200, numCams = 20, pixelNoise = 1, fov = 50*degree, minDist = 2, maxDist = 3}
    infoSProb' pixFactor s

lQ (a,b,c) = a
hQ (a,b,c) = c
mQ (a,b,c) = b

sortLinks s = map f [0..snC s -1] where
    f k = sortBy (compare `on` (negate . length . commonPoints s k)) ([0..snC s -1] \\ [0..k])

-------------------------------------------------------------

distCen s i j = norm (c j - c i) where
    c = inHomog . nullVector . (sCam s !!)

------------------------------------------------------------

-- xjT E xi
relativeE s i j = (r,c) where
    esen = trans . reshape 3 . last . toColumns . snd . rightSV . fromJust . lookup (i,j) $ epiObs s
    k  = head (commonPoints s i j)
    p  = toList $ inHomog $ ako s (k,i)
    p' = toList $ inHomog $ ako s (k,j)
    ms = camerasFromEssential esen
    m' = selectCamera p p' cameraAtOrigin ms
    (_,r,c) = factorizeCamera m'

extrapolate s i j d = fromBlocks [[r2, -asColumn (r2 <> c2)]] where
    (r,c) = relativeE s i j
    (_,r1,c1) = factorizeCamera (sCam s !! i)
    r2 = r <> r1
    c2 = c1 + trans r1 <> (LA.scalar d * c)

extrapolateEpi s k = extrapolate s (k-1) k 1


initStart2 s = s { sCam = replaceAt [0] [cameraAtOrigin] (sCam s) }


-- bootstrapEpi used free s = recompPts $ boot s (snC s -1) where
--     boot s 0 = initStart2 s
--     boot s k = step  (boot s (k-1)) k
-- 
--     step s k = result where
--         ck = extrapolate s (k-1) k 1
--         so = s {sCam = replaceAt [k] [ck] (sCam s)}
--         --so' = gea [(max 0 (k-2)) .. k][k] so
--         result = gea [(max 0 (k-used+1)) .. k][(max 1 (k-free+1)) .. k] so

initStart3 s = s { sCam = replaceAt [0,1] [cameraAtOrigin,c1] (sCam s) } where
    s' = initStart2 s
    c1 = extrapolate s' 0 1 1

-- bootstrapTri used free s = recompPts $ boot s (snC s -1) where
--     boot s 1 = initStart3 s
--     boot s k = step  (boot s (k-1)) k
-- 
--     step s k = result where
--         ck = extrapolateTri s k
--         so = s {sCam = replaceAt [k] [ck] (sCam s)}
-- --        so' = gea [(max 0 (k-2)) .. k][k] so
--         result = gea [(max 0 (k-used+1)) .. k][(max 1 (k-free+1)) .. k] so

extrapolateTri s k = extractFirst (p2d $ tensorSelectK [k,k-2,k-1] s) [sCam s !!(k-2), sCam s!!(k-1)]

extrapolateResect s k = sCam (recompCamsSel s [0..k-1] [k]) !! k

extrapolateUnif s k = unifmov (sCam s!!(k-2)) (sCam s!!(k-1))

bootStrapGen guess refine s = recompPts $ boot s (snC s -1) where
    boot s 1 = initStart3 s
    boot s k = step  (boot s (k-1)) k

    step s k = refine k so where
        ck = guess s k
        so = s {sCam = replaceAt [k] [ck] (sCam s)}

refine1 used free k =  gea [(max 0 (k-used+1)) .. k] [(max 1 (k-free+1)) .. k] . gea [0..k] [k]

bootstrapResect used free = bootStrapGen extrapolateResect (refine1 used free)
bootstrapTrifocal used free = bootStrapGen extrapolateTri (refine1 used free)
bootstrapEssential used free = bootStrapGen extrapolateEpi (refine1 used free)
bootstrapUnif used free = bootStrapGen extrapolateUnif (refine1 used free)

----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------


someHelix = sparseFromTensor $ mkHelix stdprob {numPoints = 100, numCams = 30, pixelNoise = 0.5, fov = 50*degree, minDist = 1, maxDist = 3} 777

