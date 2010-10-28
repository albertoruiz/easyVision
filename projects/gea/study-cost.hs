import Vision hiding (degree,rot1,rot2,rot3,infoSProb)
import Vision.Multiview
import Vision.Gea

import Text.Printf
import Data.List
import Data.Function(on)
import Util.Misc
import Numeric.LinearAlgebra
import qualified Data.Array as A

import EasyVision hiding (debug, numCams,(|+|), median)
import Data.Colour.Names as Col
import Graphics.UI.GLUT hiding (Size,scale,triangulate)
import Data.Maybe(fromJust)
import Control.Arrow((&&&))

s2th = 0.7

-- protectPairs p = p { epiObs = debug "filt: " length $ filter q (debug "orig: " length $ epiObs p) } where
--     q (_, m) = s2 > s2th
--         where e = linearEssen m
--               (s2,s3) = qEssen e


pairwise n p = do
    let ps = map ( pairReco.snd) $ essentialPairs n p
    runIt $ do
        shRecosG fst snd "pairwise" ps
        shRecosG fst snd "relocated" (map relocateReco ps)

studyPairs n p = do
    infoSProb p
    putStr "min points, pairs: "; print (n, withAtLeast n p)
    print $ length $ {-debug "ok: " id $ -} map fst $  essentialPairs' n p
    let ps = selectedRots n p
        (_,resi) = residualRots ps
        (sol,err) = solveEcs resi
    putStr "Lie errors: "; print $ err
    let (r0,r) = initRots n p
        p' = p { sCam = zipWith repRot (sCam p) r0 }
        p'' = p { sCam = zipWith repRot (sCam p) r }
    putStr "orig Q "; rotationQuality n p
    putStr "base Q "; rotationQuality n p'
    putStr "Lie  Q "; rotationQuality n p''
    study n p

repRot cam r = k <> r <> c' where
    (k,_,c) = factorizeCamera cam
    c' = ident 3 & asColumn (-c)


study n p = do
    let q = mySparseBundle p 10
        (r0,r) = initRots1I n (rotOfCam (sCam q!!0)) p
        q' = geaFull $ q { sCam = zipWith repRot (sCam q) r }
    rotationQuality n q
    infoCosts q
    rotationQuality n q'
    infoCosts q'
    runIt $ shRecos "rec" $ map relocateSparse [p,q',q]

main = do
    studyHelix
    compareMethods "corridor"


compareMethods name = do
    p <- loadData name
    runIt $ shRecos "init" [ p 
                           , (geaFull p)
                           , (mySparseBundle p 10)]


geaux n s = recompPts . fst . debug "" snd . geaG 1 n 0.001 used free $  s
    where used = sel
          free = sel
          sel = [0.. snC s -1]

someHelix = sparseFromTensor $ mkHelix stdprob
    { numPoints = 100
    , numCams = 30
    , pixelNoise = 1
    , fov = 50*degree
    , minDist = 1
    , maxDist = 3
    } 777

pertu = someHelix { sCam = rotate (sCam someHelix) }
    where rotate (b:bs) = bs++[b]

studyHelix = do
    let p = someHelix
    infoSProb p
    infoCosts p
    infoCosts pertu
    putStrLn "--------------------------"
    let r = geaFull . geaux 1 $ pertu
    infoCosts r
    let s = mySparseBundle pertu 10
    infoCosts s
    let t = sbaG 6 39 pertu 10
    infoCosts t

infoCosts s = do
    let pixFact = fromIntegral (sRes s) / 2
    printf "geom dist    = %.1f\n" (snd (sGError s))
--    printf "epi dist     = %.1f\n" (snd (sEError s)*pixFact)
--    printf "empiric epi  = %.1f\n" (snd (sEEError s)*pixFact)
--    printf "algeb epi    = %.1f\n" (snd (sEAError s))

shProbS tit s = shProb tit $ relocate $ (tensorProblemK [0..snP s -1] [0..snC s -1] s)

loadData pname = do
    let [fp,fc,fk] = map f ["pts.txt","cams.txt","calib.txt"]
            where f n = "../../data/tracks/"++pname++"/"++n
    p <- loadSVP fp fc fk
    return p {sRes = 640}

-------------------------------------------------------------

runIt f = prepare >> f >> mainLoop

shRecoG fc fp name s = do
    evWin3D () name 500 (Just disp) (const kbdQuit)
    clearColor $= Color4 1 1 1 1
  where
    disp st = do
        setColor' black
        mapM_ (flip (drawCamera 0.1) Nothing) (fc s)
        pointSize $= 1
        setColor' blue
        renderPrimitive Points $ mapM_ (vertex.toList.inHomog) (fp s)

--------------------------------------------------------------

-- ok, but we must align the reconstructions

-- and precompute something?

shRecosG fc fp name ss = do
    evWin3D 0 name 500 (Just disp) (mouseGen acts kbdQuit)
    clearColor $= Color4 1 1 1 1
  where
    disp st = do
        k <- get st
        setColor' black
        mapM_ (flip (drawCamera 0.1) Nothing) (fc $ ss!!k)
        pointSize $= 1
        setColor' blue
        renderPrimitive Points $ mapM_ (vertex.toList.inHomog) (fp $ ss!!k)
    acts = [((SpecialKey KeyUp,   Down, modif), \k -> min (k+1) n)
           ,((SpecialKey KeyDown, Down, modif), \k -> max (k-1) 0)]
    n = length ss - 1

shReco = shRecoG sCam sPts
shRecos = shRecosG sCam sPts

----------------------------------------------------------------
-- things to move to SparseRep

essentialPairs' n = essentialPairs q
    where q (m,s1,s2) = m>=n && s1 > s2th

essentialPairs :: ((Int, Double, Double) -> Bool)
               -> SparseVP
               -> [((Int, Int),(Mat, ([[Double]], [[Double]])))]
essentialPairs q s = map snd $ filter (q.fst) $ map f $ essentials s where
    f ((i,j),(e,ks)) = ((length ks,s2,s3),((i,j), (e, (ps, ps'))))
      where
        (s2,s3) = qEssen e
        ps  = map (\k -> toList $ inHomog $ ako s (k,i)) ks
        ps' = map (\k -> toList $ inHomog $ ako s (k,j)) ks

--xjT E xi
relativeRotation (esen,(ps,ps')) = r where
    ms = camerasFromEssential esen
    m' = selectCamera (head ps) (head ps') cameraAtOrigin ms
    (_,r,_c) = factorizeCamera m'

relativeRotation' x = r where
    ([_,m'],_) = pairReco x
    (_,r,_c) = factorizeCamera m'

relativeRotation2 (esen,(ps,ps')) = (r1,r3) where
    ms = camerasFromEssential esen
    (_,r1,_) = factorizeCamera (ms!!0)
    (_,r3,_) = factorizeCamera (ms!!2)




-- devolvemos la que tenga m´as puntos delante, por ruido no
-- nos podemos fiar solo de uno de ellos
pairReco (esen,(ps,ps')) = ([m,m'], map (homog.vec) pts) where
    ms = camerasFromEssential esen
    m = cameraAtOrigin
    ptss = map (\c -> triangulate [(m,ps),(c,ps')]) ms
    (m', pts) = maximumBy (compare `on` front) $ zip ms ptss
    front (c,pts) = length $ filter g pts
        where g p =  depthOfPoint p cameraAtOrigin > 0
                  && depthOfPoint p c > 0



rotationQuality n p = do
    shDist "rot: " "%.3f" "%.3f" qs
  where qs = rotQuality n p
        qs' = rotQuality2 n p
        qs'' = map snd qs'

withAtLeast k p = length $ filter (>=k) $ map f $ epiObs p where
    f ((i,j),_) = length $ (commonPoints p i j)



relocateSparse p = p { sPts = newPts, sCam = newCams } where
    (newCams, newPts) = relocateReco (sCam p, sPts p)

relocateReco (cams,pts) = (newCams, newPts) where
    newPts = toRows $ (fromRows pts) <> trans hi
    newCams = map (<>h) cams
    hi = inv h
    h = (4><4) [s,0,0,x,
                0,s,0,y,
                0,0,s,z,
                0,0,0,1]
    things = ihmat . fromRows $ pts
    (m,c) = meanCov things
    [x,y,z] = toList m
    s = sqrt $ maxElement $ eigenvaluesSH' c


hmat m = fromBlocks [[m,1]]
ihmat m = takeColumns c m / dropColumns c m where c = cols m  - 1


assert msg cond x = if cond x then x else error $ msg ++ show x


rotQuality2 n p = map f (essentialPairs' n p) where
    f ((i,j),x) = ((i,j), minimum [a1,a2])
        where (r1,r3) = relativeRotation2 x
              a1 = angularError r1 (rot j <> trans (rot i))
              a2 = angularError r3 (rot j <> trans (rot i))
    rot = arrayOf (map g (sCam p)) where g c = r where (_,r,_) = factorizeCamera c

rotQuality n p = map f (essentialPairs' n p) where
    f ((i,j),x) = angularError (relativeRotation x) (rot j <> trans (rot i))
    rot = arrayOf (map g (sCam p)) where g c = r where (_,r,_) = factorizeCamera c

angularError x y = sum $ zipWith aang xs ys
      where
        xs = toRows x
        ys = toRows y
        aang u v = acos (min 1 $ abs $ u <.> v) / degree

--------------------------------------------

selectedRots n p = sortBy (compare `on` s3.fst.snd) $ map f (essentialPairs n p) where
    f ((i,j),x) = ((i,j), (toList $ singularValues $ fst x,relativeRotation x))
    s3 [a,b,c] = c/a


-- by now contiguous in the sequence
baseRots sel = g where
    f 0 = ident 3
    f k = (snd $ fromJust $ lookup (k-1,k) sel) <> f (k-1)
    fs = map f [0..]
    g = (fs!!)

-------------------------------------------------------------
-- best fit of rotations in tangent space

residualRots sel = (r, map f sel) where
    r = baseRots sel
    f ((i,j),(_,rel)) = ((i,j), coordsRot $ trans (r j) <> rel <> r i)

makeEc n (i,j) = replicate i 0 ++ [1] ++ replicate (j-i-1) 0 ++ [-1] ++ replicate (n-j) (0::Double)

makeEcs resi = (fromLists coef, fromLists desi)
    where coef = map (makeEc n . fst) resi
          n = maximum $ map (snd.fst) resi
          desi = map snd resi

solveEcs resi = (sol,err)
  where
    (c,d) = makeEcs resi
    sol = c `linearSolveSVD` d
    err = (norm (c <> sol - d), norm d)
    norm = pnorm Frobenius

initRots n p = (r0, r)
  where ps = selectedRots n p
        (r0',resi) = residualRots ps
        (sol,err) = solveEcs resi
        r0 = map r0' [0..length (sCam p)-1]
        r = zipWith fixRot r0 (toLists sol)

initRots1I n b p = (f r0, f r)
  where (r0,r) = initRots n p
        f rs = map (<> (trans (head rs) <> b)) rs

rotOfCam c = r where (_,r,_) = factorizeCamera c

fixRot r [a,b,c] = r <> trans dr
    where --dr = expm (scalar a * g1 |+| scalar b * g2 |+| scalar c * g3 )
          dr = expm (scalar a * g1 + scalar b * g2 + scalar c * g3)

-----------------------------------------------------------------------------------

logm m = fst . fromComplex . matFunc log . complex $ m

rot1, rot2, rot3 :: Double -> Mat
rot1 a = (3><3)
 [ 1, 0, 0
 , 0, c, s
 , 0,-s, c ]
    where c = cos a
          s = sin a

rot2 a = (3><3)
 [ c, 0, s
 , 0, 1, 0
 ,-s, 0, c ]
    where c = cos a
          s = sin a

rot3 a = (3><3)
 [ c, s, 0
 ,-s, c, 0
 , 0, 0, 1 ]
    where c = cos a
          s = sin a


g1,g2,g3 :: Mat

g1 = (3><3) [0, 0,0
            ,0, 0,1
            ,0,-1,0]

g2 = (3><3) [ 0,0,1
            , 0,0,0
            ,-1,0,0]

g3 = (3><3) [ 0,1,0
            ,-1,0,0
            , 0,0,0]

coordsRot r = [a,b,c] where
    [[_,c,b],
     [_,_,a],
     [_,_,_]] = toLists (logm r)

infix 8 ~&~
a ~&~ b = a <> b - b <> a

infixl 6 |+|
a |+| b = a + b + a ~&~ b /2  + (a-b) ~&~ (a ~&~ b) /12

------------------------------------------------------------------
-- testing data

gr a b c = rot3 (c*degree) <> rot2 (b*degree) <> rot1 (a*degree)

tr 0 = gr 0 0 0
tr 1 = gr 30 40 50
tr 2 = gr (-10) 20 (-30)
tr 3 = gr 55 (-30) 45

dr 0 = gr 1 (-2) 3
dr 1 = gr 5 5 (-7)
dr 2 = gr 0 8 2
dr 3 = gr (-5) 4 (-2)

br k = tr k <> dr k

obs = [(0,1),(0,2),(1,2),(1,3),(2,3)]
--obs = [(0,1),(1,2),(2,3)]

residualRotsTesting = (br, map f obs) where
    f (i,j) = ((i,j), coordsRot $ trans (br j) <> rij <> br i) where rij = tr j <> trans (tr i)

(r0',resi) = residualRotsTesting
(coef,desi) = makeEcs resi
(sol,err) = solveEcs resi
r0 = map r0' [0..(maximum $ map snd obs)]
ropt' = zipWith fixRot r0 (toLists sol)
ropt = ((map (<>trans (ropt'!!0)) ropt')!!)

err0 = sum $ zipWith angularError (map tr [0..3]) (map (<> trans (r0!!0)) r0)
err1 = sum $ zipWith angularError (map tr [0..3]) (map ropt [0..3])

