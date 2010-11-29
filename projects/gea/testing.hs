import Vision hiding (degree,rot1,rot2,rot3,infoSProb)
import Vision.Multiview
import Vision.Gea

import Text.Printf(printf)
import Data.List
import Data.Function(on)
import Util.Misc
import Numeric.LinearAlgebra
import qualified Data.Array as A

--import EasyVision hiding (debug, numCams,(|+|), median)
--import Data.Colour.Names as Col
--import Graphics.UI.GLUT hiding (Size,scale,triangulate)
import Data.Maybe(fromJust,isJust)
import Control.Arrow((&&&))
import System.Random(randomIO)

import LieSolve
import Initialize
import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util

import ShowReco

--------------------------------------------------------------

main = mapM_ f ["dinosaur"
               ,"trafalgar-21"
               ,"maquette"
               ,"boxes-6"
               ,"wardham_college"
               ,"model_house"
               ,"corridor"]
  where  f s = loadData s >>= testNew 30 0.9 0 40 0 10000


--------------------------------------------------------------




someHelix = sparseFromTensor $ mkHelix stdprob
    { numPoints = 200
    , numCams = 30
    , pixelNoise = 1
    , fov = 50*degree
    , minDist = 4
    , maxDist = 5
    } 77799

{- compare h_err & s7:
   mplot . return . fromList $ map (\i-> s7 $ fromJust $ lookup (0,i) $ epiObs someHelix) [1..29]
-}

infoCosts s =  printf "geom dist    = %.1f\n" (snd (sGError s))
            >> printf "kal  dist    = %.2f\n" (snd (kError s))

disp d = putStr . dispf d



loadData pname = do
    let [fp,fc,fk] = map f ["pts.txt","cams.txt","calib.txt"]
            where f n = "../../data/tracks/"++pname++"/"++n
    p <- loadSVP fp fc fk
    return p

display name = do
    p <- loadData name
    let fp = flipProb p
    print $ sGError p
    print $ sGError fp
    runIt $ shRecos "cosa" (map relocateSparse [p,fp])

flipProb p = p { sCam = flipc (sCam p), sKal = id (sKal p), lObs = flipp (lObs p) }

flipc = map (diag (fromList[1,-1,1]) <>)
--flipk = map (diag (fromList[1,-1,1]) <>)
flipp = map (\(ij,[x,y]) -> (ij, [x,-y]))



test p = do
    putStr "given: "; infoCosts p
    let gea0 = geaFull p
    --putStr "gea given: "; infoCosts gea0
    let q = bootstrap0 p
        gea1 = geaFull q
    putStr "boot0: "; infoCosts q
    --putStr "gea boot0: "; infoCosts gea1
    let q3 = bootstrap3 0.9 30 p
        gea4 = geaFull q3
    putStr "boot3: "; infoCosts q3
    let q4 = bootstrapDummy' 5 0.9 30 p
    putStr "pts-cens: "; infoCosts q4

    --putStr "gea boot2: "; infoCosts gea3

{-    --mapM_ print $ detailedErrors gea0
    --    runIt $ shRecos "given" (map relocateSparse [p,gea0])
    --          >> shRecos "bootstrap" (map relocateSparse[q,gea1])
    --    infoCosts (mySparseBundle p 10)
    --    infoCosts (mySparseBundle q 10)
    --    infoCosts (geaFull q)
-}    
    runIt $ shRecos "cosa" (map relocateSparse [p,q,q3,q4])



-- testing the estimation of centers
testCenters s2' n' p = t where
    nc = length (sCam p)
    sel = filter ((>n').nEpi.snd)
        . filter ((>s2').s2.snd)
        . filter (isJust.rot.snd) 
        $ epiObs p
    rots = mapSnd (fromJust.rot) sel
    arcs1 = map fst $ sortBy (compare `on` negate.s2.snd) sel
    span1 = kruskal (nc - 1) arcs1
    r0 = graphInit span1 rots
    rotsel = mapSnd (fromJust.rot) sel
    rs = rotRefine 1 50 (rotRefine 1 50 r0 rotsel) rotsel
    q = debug "init: " (snd.kError) $ selectSol (solveCams rs (epiObs p)) p
    gea = debug "gea: " (snd.kError) $ geaFull q
    bun = debug "bun: " (snd.kError) $ mySparseBundle gea 10
    trueRots = map rotOfCam (sCam bun)
    t = debug "comprots:" (const (map (round.(*100)) $ compareRots' trueRots rs)) $ selectSol (solveCams trueRots (epiObs p)) p

---------------------------------------------------------

fw o = id (fromIntegral (nEpi o))

testNew n s2' s7' ang n1 n2 p = do
    infoSProb p
    let nc = length (sCam p)
        f (ij,x) = nEpi x > n && s2 x > s2' && s7 x > s7' && isJust (rot x)
        sel = filter f (epiObs p)                        -- selected subgraph
        rots = mapSnd (fromJust.rot) sel                 -- extract rotations
        weights = vec (map (fw.snd) sel)                 -- extract weigths
    printf "selected pairs: %d\n" $ length sel
    let arcs1 = map fst $ sortBy (compare `on` negate.s2.snd) sel
        span1 = kruskal (nc - 1) arcs1
        arcs2 = filter (not . (`elem` span1)) arcs1
        span2 = kruskal (nc - 1) arcs2
--    putStr "span1: "; print span1
--    putStr "span2: "; print span2
    let r0 = graphInit span1 rots
        r1 = graphInit span2 rots
    compareRots "graph difference: " r0 r1
    --mapM_ (print.path span1 0) [0.. nc-1]
    --mapM_ (print.path span2 0) [0.. nc-1]
    let rs = rotRefine 1 ang r0 rots
--    let rs1 = rotRefine ang r1 rots
    let rots2graphs = mapSnd (fromJust.rot) $ filter ((`elem` (span1 ++ span2)).fst) sel
        rs2graphs = rotRefine 1 ang r0 rots2graphs
    let f2 x = nEpi x > n1 && nEpi x < n2
        q = selectSol (solveCams rs (filter (f2.snd) (epiObs p))) p
        --q = selectSol (solveCams rs2graphs (filter ((`elem` (span1 ++ span2)).fst) (epiObs p))) p
    infoCosts q
    mapM_ print $ sortBy (compare `on` negate . snd) $ detailedErrors q
    let g = geaFull q
    infoCosts g
    let s = mySparseBundle g 10
    infoCosts s
    let rt = map rotOfCam (sCam s)
    compareRots "bundle vs all " rt rs
    compareRots "bundle vs 2 graphs "rt rs2graphs
    let qt = selectSol (solveCams rt (filter (f2.snd) (epiObs p))) p
    infoCosts qt
    let s0 = mySparseBundle p 60
    putStr "bundle: "; infoCosts s0
    let g0 = geaFull p
    putStr "gea: "; infoCosts g0
    let s1 = mySparseBundle g0 10
    putStr "gea+bundle: "; infoCosts s1
    runIt $ shRecos "cosa" (map relocateSparse [q,p,g,s])




compareRots tit r0s r1s = shDist tit "%.2f" "%.1f"$ compareRots' r0s r1s

compareRots' r0s r1s = map snd $ zipWith3 (\n a b -> (n,pnorm PNorm2 (vec $ coordsRot (a <> trans b)) / degree)) [0..] r0s r1s
--------------------------------------------------------------



--prop = negate.length.snd
prop = negate.fst.qEssen.fst

testGraph p = do
    let ps = sortPairs prop p
        t = kruskal (length (sCam p) - 1) (map fst ps)
    let f ps ij = let Just x = lookup ij ps in prop x
        (a,b) = head t
    print $ map (f ps) t `zip` t
    print $ t
    print $ sortBy (compare `on` snd) [ ((i,j),prop x) | ((i,j),x) <- ps , a==i || a==j ]
    print $ sortBy (compare `on` snd) [ ((i,j),prop x) | ((i,j),x) <- ps , b==i || b==j ]
--    runIt $ shRecosG fst snd "worst pair" (pairReco p (head t))
--         >> shRecosG fst snd "best pair"  (pairReco p (last t))


testNewGraph p = do
    let epi = epiObs p
        ps = filter (superOk p) $ sortBy (compare `on` (negate.s2.snd)) $ filter ((>10).nEpi.snd) $ epi
        t = kruskal (length (sCam p) - 1) (map fst ps)
        f ps ij = let Just x = lookup ij ps in s2 x
        (a,b) = head t
    print $ map (f ps) t `zip` t
    print $ t
    print $ sortBy (compare `on` negate.snd) [ ((i,j), s2 x) | ((i,j),x) <- ps , a==i || a==j ]
    print $ sortBy (compare `on` negate.snd) [ ((i,j), s2 x) | ((i,j),x) <- ps , b==i || b==j ]
--    runIt $ shRecosG fst snd "worst pair" (pairReco p (head t))
--         >> shRecosG fst snd "best pair"  (pairReco p (last t))

-----------------------------------------------------------

perturba seed s1 s2 p = recompPts p { sCam = noisycams } where
    n = length (sCam p)
    rand = reshape 3 $ randomVector seed Gaussian (3*2*n)
    f v = rot1 (v@>0 * degree) <> rot2 (v@>1 * degree) <> rot3 (v@>2 * degree)
    mr = map f $ toRows $ scale s1 $ takeRows n rand
    mc = toRows $ scale s2 $ dropRows n rand
    h cam1 cam2 = pnorm PNorm2 (c1-c2)
        where (_,_,c1) = factorizeCamera cam1
              (_,_,c2) = factorizeCamera cam2
    boxSize = maximum $ pairsWith h (sCam p)
    g cam q d = fromBlocks [[rt, asColumn (-rt <> ct)]]
        where (_,r,c) = factorizeCamera cam
              rt = q <> r
              ct = c+ scale boxSize d
    noisycams = zipWith3 g (sCam p) mr mc


studyPertu p s1 s2 = do
    putStr "initial: "; infoCosts p
    seed <- randomIO
    let q = perturba seed s1 s2 p
    putStr "perturbated: "; infoCosts q
    let g = geaFull q
    putStr "gea: "; infoCosts g
    let s = mySparseBundle q 10
    putStr "bundle: "; infoCosts s
    runIt $ shRecos "cosa" (map relocateSparse [p,q,g,s])

-----------------------------------------------------------


((1,4),x) = mapSnd m_hat (epiObs someHelix) !! 6
cams = sCam someHelix
(_,ri,ci) = factorizeCamera (cams!!1)
(_,rj,cj) = factorizeCamera (cams!!4)
rots = map f cams where f c = let (_,r,_) = factorizeCamera c in r
cens = map f cams where f c = let (_,_,q) = factorizeCamera c in q


((1,4),(e,_)) = essentials someHelix !! 6

cij = asMat (ci - cj)
eij = ri <> cij <> trans rj

ok = maxElement $ x <> flatten (trans e)
ok2 = maxElement $ x <> flatten (eij)
ok3 = maxElement $ coords $ t * rit!"ai" * rjt!"bj" * tcen!"ab"
ok4 = (rows ur, maxElement $ ur <> (ci-cj))
ok5 = (rows ccc, maxElement $ ccc <> join cens)

t = (!"kij").listArray[rows x,3,3].toList $ flatten x
rit = trot ri
rjt = trot rj

trot = listArray[3,3].toList.flatten .trans
tcen = trot cij

((1,4),ur) = unRotEpi rots (mapSnd m_hat (epiObs someHelix)) !! 6

ccc = coeffCent rots (mapSnd m_hat (epiObs someHelix))
