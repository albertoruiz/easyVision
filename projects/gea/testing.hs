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

import LieSolve
import Initialize
import Numeric.LinearAlgebra.Array
import Numeric.LinearAlgebra.Array.Util

import ShowReco

--------------------------------------------------------------

main = mapM_ f ["dinosaur",
                "maquette",
                "boxes-6",
                "wardham_college",
                "model_house"]
  where  f s = loadData s >>= testNew 30 0.9 1E-5 40


--------------------------------------------------------------




someHelix = sparseFromTensor $ mkHelix stdprob
    { numPoints = 200
    , numCams = 30
    , pixelNoise = 2
    , fov = 50*degree
    , minDist = 1
    , maxDist = 3
    } 77799

infoCosts s = printf "geom dist    = %.1f\n" (snd (sGError s))

disp d = putStr . dispf d

loadData pname = do
    let [fp,fc,fk] = map f ["pts.txt","cams.txt","calib.txt"]
            where f n = "../../data/tracks/"++pname++"/"++n
    p <- loadSVP fp fc fk
    return p

test s2 p = do
    infoSProb p
    --testGraph p
    let q = bootstrap s2 p
    infoCosts p
    infoCosts q
    let gea0 = geaFull p
    infoCosts gea0
    let gea1 = geaFull q
    infoCosts gea1
    --mapM_ print $ detailedErrors gea0
    runIt $ shRecos "cosa" (map relocateSparse [q,p,gea0,gea1])
--    infoCosts (mySparseBundle p 10)
--    infoCosts (mySparseBundle q 10)
--    infoCosts (geaFull q)


-- | geometric reprojection error of a sparse visual problem
sGErrorView :: Int -> SparseVP -> (Double,Double)
sGErrorView k s = (2*e2, sqrt e2) where
    obs = filter (\((_,j),_)->j==k) (lObs s)
    n = fromIntegral (length obs)
    c = arrayOf (zipWith (<>) (sKal s) (sCam s))
    p = arrayOf (sPts s)
    g ac ((i,j), [x,y]) = ac + sqr(x-a/w) + sqr(y-b/w)
        where [a,b,w] = toList $ c j <> p i
    g _ _ = impossible "sGError"
    e2 = foldl' g 0 obs / (2*n)

detailedErrors s = map f [0..length (sCam s)-1] where
    f k = (k, snd (sGErrorView k s))



---------------------------------------------------------


testNew n s2' s7' ang p = do
    infoSProb p
    let nc = length (sCam p)
        f (ij,x) = nEpi x > n && s2 x > s2' && s7 x > s7' && isJust (rot x)
        sel = filter f (epiObs p)                        -- selected subgraph
        rots = mapSnd (fromJust.rot) sel                 -- extract rotations
    printf "selected pairs: %d\n" $ length sel
    let arcs1 = map fst $ sortBy (compare `on` negate.s2.snd) sel
        span1 = kruskal (nc - 1) arcs1
        arcs2 = filter (not . (`elem` span1)) arcs1
        span2 = kruskal (nc - 1) arcs2
--    putStr "span1: "; print span1
--    putStr "span2: "; print span2
    let r0 = graphInit span1 rots
        r1 = graphInit span2 rots
    --mapM_ print $ zipWith3 (\n a b -> (n,pnorm PNorm2 (vec $ coordsRot (a <> trans b)) / degree)) [0..] r0 r1
    --mapM_ (print.path span1 0) [0.. nc-1]
    --mapM_ (print.path span2 0) [0.. nc-1]
    let rs = rotRefine ang r0 rots
--    let rs1 = rotRefine ang r1 rots
    let q = selectSol (solveCams rs (epiObs p)) p
    infoCosts q
    runIt $ shRecos "cosa" (map relocateSparse [q])


graphInit t rots = paths
  where paths = ident 3 : map (chain . path t 0) [1.. maximum (map snd t)]
        chain xs = foldl1' (<>) $ map f $ zip xs (tail xs)
        f (i,j) = case lookup (i,j) rots of
                     Just r -> r
                     Nothing -> case lookup (j,i) rots of
                                   Just r -> trans r
                                   Nothing -> error $ "graphInit" ++ show (i,j)


rotRefine angMax r0 sel = debug "rotation system error gain: " (const err) rs where
    r = arrayOf r0
    f ((i,j),rel) = ((i,j), coordsRot $ trans (r j) <> rel <> r i)
    d = debug "rot residuals: " (map (round.h.snd))
    h = (/degree). pnorm PNorm2 . vec
    g = filter ((<angMax).h.snd) . debug "removed: " (map fst . filter ((>=angMax).h.snd))
    resi = d . g $ map f sel
    (sol,err) = solveEcs resi
    rs = zipWith fixRot r0 (toLists sol)


solveCams rs sel = (cams1, cams2) where
    cens = fst $ debug "centers system error: " snd $ estimateCenters rs sel
    cams1 = zipWith f rs cens
    cams2 = zipWith f rs (map negate cens)
    f r c = r & asColumn (-r <> c)

selectSol (cams1,cams2) p = if looksRight sol1 then sol1 else sol2 where
    sol1 = recompPts p { sCam = cams1 }
    sol2 = recompPts p { sCam = cams2 }

-- if looksRight sol1 then sol1 else sol2 where
--     
--     sol1 = recompPts p { sCam = cams1 }
--     sol2 = recompPts p { sCam = cams2 }

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


