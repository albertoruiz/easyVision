module Initialize where

import LieSolve

import Numeric.LinearAlgebra
import Util.Misc(Mat,arrayOf,Vec,homogSolve,(&),debug,vec,diagl,replaceAt,intersectSorted,
                 degree)
import Vision(factorizeCamera,camerasFromEssential,selectCamera,cameraAtOrigin,
              homog,inHomog, commonPoints, triangulate,
              SparseVP,ako,essentials,qEssen,epiObs,sCam,depthOfPoint,depthsOfInducedPoint,
              v_of_p, sPts, Epi(..))
import Vision.Gea(recompPts)
import Numeric.LinearAlgebra.Tensor
import Numeric.LinearAlgebra.Array.Util
import Data.Maybe(isJust,fromJust)
import Data.List(sortBy,sort,nub,foldl',foldl1',maximumBy)
import Data.Function(on)

-- initialization of rotations in a sparse visual problem

rotOfCam :: Mat -> Mat
rotOfCam c = r where (_,r,_) = factorizeCamera c

linearEssen :: Mat -> Mat
linearEssen = trans . reshape 3 . last . toColumns . snd . rightSV


-- devolvemos la que tenga m´as puntos delante, por ruido no
-- nos podemos fiar solo de uno de ellos
pairReco s (i,j) = map f sorted where
    ms = camerasFromEssential $ esen $ debug "sv: " (singularValues.m_hat) $ fromJust $ lookup (i,j) (epiObs s)
    ks = debug "length: " length $ commonPoints s i j
    ps  = map (\k-> toList $ inHomog $ ako s (k,i)) ks
    ps' = map (\k-> toList $ inHomog $  ako s (k,j)) ks
    m = cameraAtOrigin
    ptss = map (\c -> triangulate [(m,ps),(c,ps')]) ms
    sorted = sortBy (compare `on` (negate.front)) $ zip ms ptss
    front (c,pts) = debug "front: " id $ length $ filter g pts
        where g p =  depthOfPoint p m > 0
                  && depthOfPoint p c > 0
    f (m',pts) = ([m,m'], map (homog.vec) pts)


-- returns both possibilities
extractRotations :: Mat -> (Mat,Mat)
extractRotations esen = (rotOfCam m0, rotOfCam m2) where
    [m0,_,m2,_] = camerasFromEssential esen


-- returns the good one but may fail
extractRotation :: Mat -> Vec -> Vec -> Mat
extractRotation esen p p' = r where
    ms = camerasFromEssential esen
    m' = selectCamera (f p) (f p') cameraAtOrigin ms
    r = rotOfCam m'
    f = toList . inHomog -- we should also admit triangulation of homog points

-- returns Maybe rot
extractRotation' :: Mat -> Vec -> Vec -> Maybe Mat
extractRotation' esen p p' = rotOfCam `fmap` m' where
    ms = camerasFromEssential esen
    m' = selectCamera' (f p) (f p') cameraAtOrigin ms
    f = toList . inHomog

-- returns Maybe camera
selectCamera' :: [Double] -> [Double] -> Mat -> [Mat] -> Maybe Mat
selectCamera' p p' m ms = m' where
    m's = filter f ms
    f m' = a>0 && b >0 where (a,b) = depthsOfInducedPoint p p' m m'
    m' | null m's  = Nothing
       | otherwise = Just (head m's)

relativeRotations :: SparseVP -> [((Int, Int), (Mat, [Int]))] -> [((Int, Int), Mat)]
relativeRotations s = map f where
    f ((i,j), (e, k:_)) = ((i,j), r)
        where r = extractRotation e p p'
              p  = ako s (k,i)
              p' = ako s (k,j)

relativeRotations' :: SparseVP -> [((Int, Int), (Mat, [Int]))] -> [((Int, Int), Mat)]
relativeRotations' s = g . map f where
    f ((i,j), (e, k:_)) = ((i,j), r)
        where r = extractRotation' e p p'
              p  = ako s (k,i)
              p' = ako s (k,j)
    g xs = debug "okpair: " length [(ij,m) | (ij, Just m) <- xs ]


-------------------------------------------------------

-- using spanning tree

residualRots p sel = (r, d . g . d $ map f sel) where
    r = spanInit p sel
    -- r = superSpanInit 30 p sel
    f ((i,j),rel) = ((i,j), coordsRot $ trans (r j) <> rel <> r i)

    d = debug "rot residuals: " (map (round.h.snd))
    h = (/degree). pnorm PNorm2 . vec
    g = filter ((<30).h.snd) . debug "removed: " (map fst . filter ((>=30).h.snd))

-------------------------------------------------------------

--initRots :: [((Int, Int), Mat)] -> ([Mat],[Mat])
initRots p ps = (r0, debug "err rot: " (const err) r)
  where (r0',resi) = residualRots p ps
        (sol,err) = solveEcs resi
        r0 = map r0' [0..maximum (map (snd.fst) ps)]
        r = zipWith fixRot r0 (toLists sol)

--initRots1I :: Mat -> [((Int, Int), Mat)] -> ([Mat],[Mat])
initRots1I p b ps = (f r0, f r)
  where (r0,r) = initRots p ps
        f rs = map (<> (trans (head rs) <> b)) rs

prepareRotsGen q p = initRots1I p (diagl [1,1,1]) . relativeRotations' p . filter q . essentials $ p

-- using only pair with minimum s2
prep s2 = prepareRotsGen (\(_,(e,ks)) -> fst (qEssen e) > s2)

-------------------------------------------------------

bootstrap s2 p = if looksRight sol1 then sol1 else sol2 where
    rots = snd $ prep s2 p
    cens = fst $ debug "err cen: " snd $ estimateCenters rots (epiObs p)
    cams1 = zipWith f rots cens
    cams2 = zipWith f rots (map negate cens)
    f r c = r & asColumn (-r <> c)
    sol1 = recompPts p { sCam = cams1 }
    sol2 = recompPts p { sCam = cams2 }

-------------------------------------------------------

estimateCenters rots = p . homogSolve . debug "system: " f . dropColumns 3 . coeffCent rots . mapSnd m_hat
    where p (s,err) = ((vec[0,0,0]:) . toRows . reshape 3 $ s, err)
          f m = (rows m, cols m, rank m)

mapSnd f = map (\(a,b)->(a,f b))

coeffCent rots = fromBlocks . map (return . f) . unRotEpi rots
    where f ((i,j),m) = ec i j m
          n = length rots - 1
          ec i j b = fromBlocks [ replicate i z
                               ++ [b]
                               ++ replicate (j-i-1) z
                               ++ [-b]
                               ++ replicate (n-j) z
                                ]
              where z = (rows b >< 3) [0,0..]

unRotEpi rots = map f
    where r = arrayOf $ map (listArray[3,3].toList.flatten .trans) rots
          n = length rots - 1
          f ((i,j),m) = ((i,j), (s.h) (g i j m))
          g i j m = r i!"ai" * t m * r j!"bj"
          t m = (!"kij").listArray[rows m,3,3].toList $ flatten m
          h x = reshape 9 $ coords (x~>"kab")
          s m = m <> trans sel
          sel = (3><9) [ 0,  0, 0, 0, 0, -1,  0, 1, 0,
                         0,  0, 1, 0, 0,  0, -1, 0, 0,
                         0, -1, 0, 1, 0,  0,  0, 0, 0 ]

-----------------------------------------------------------------

looksRight s = depthOfPoint p c1 > 0 && depthOfPoint p c2 > 0
  where
    v1:v2:_ = v_of_p s 0
    p = toList . inHomog $ sPts s !! 0
    c1 = sCam s !! v1
    c2 = sCam s !! v2

-----------------------------------------------------------------
-- initialization from the spanning tree

sortPairs p = sortBy (compare `on` (p.snd)) . essentials

spanInit p sel = g
  where ps = map fst $ sortPairs (negate.fst.qEssen.fst) p
        t = kruskal (length (sCam p) -1) ps
        paths = ident 3 : map (chain . path t 0) [1.. maximum (map snd t)]
        chain xs = foldl1' (<>) $ map f $ zip xs (tail xs)
        g = (paths!!)
        f (i,j) = case lookup (i,j) sel of
                     Just rot -> rot
                     Nothing -> case lookup (j,i) sel of
                                     Just rot -> trans rot
                                     Nothing -> error "spanInit"






-- devolvemos la que tenga m´as puntos delante, por ruido no
-- nos podemos fiar solo de uno de ellos
superOk s ((i,j),x) = debug "tested: " (const (i,j)) $ best == nEpi x where
    ms = camerasFromEssential $ esen x
    ks = commonPoints s i j
    ps  = map (\k-> toList $ inHomog $ ako s (k,i)) ks
    ps' = map (\k-> toList $ inHomog $ ako s (k,j)) ks
    m = cameraAtOrigin
    ptss = map (\c -> triangulate [(m,ps),(c,ps')]) ms
    best = last $ sort $ map front $ zip ms ptss
    front (c,pts) = length $ filter g pts
        where g p =  depthOfPoint p m > 0
                  && depthOfPoint p c > 0


superSpanInit nmin p sel = g
  where epi = epiObs p
        ps = filter (superOk p) $ sortBy (compare `on` (negate.s2.snd)) $ filter ((>=nmin).nEpi.snd) $ epi
        t = kruskal (length (sCam p) - 1) (map fst ps)
        paths = ident 3 : map (chain . path t 0) [1.. maximum (map snd t)]
        chain xs = foldl1' (<>) $ map f $ zip xs (tail xs)
        g = (paths!!)
        f (i,j) = case lookup (i,j) sel of
                     Just rot -> rot
                     Nothing -> case lookup (j,i) sel of
                                     Just rot -> trans rot
                                     Nothing -> error "spanInit"





-----------------------------------------------------------------

neigh g n = myunion [[ j | (k,j) <- g , k == n ],
                     [ i | (i,k) <- g , k == n ]]

myunion xs = nub . sort . concat $ xs

-- spanning tree, from sorted arcs
-- nmax is given from outside to keep laziness
kruskal :: Int -> [(Int, Int)] -> [(Int, Int)]
kruskal nmax s = fst $ myfoldl' ((nmax==).length.fst) f ([],r0) s where
    -- nmax = maximum (map snd s)
    r0 = map return [0..nmax]
    f (g,r) (i,j) = if i `elem` r!!j then (g,r) else ((i,j):g, r')
        where r' = replaceAt z (replicate (length z) z) r
              z = myunion [r!!i, r!!j]

myfoldl' done f z0 xs0 = lgo z0 xs0
    where lgo z _ | done z = z
          lgo z []     = z
          lgo z (x:xs) = let z' = f z x in z' `seq` lgo z' xs


--path from a to b in a tree
path :: [(Int,Int)] -> Int -> Int -> [Int]
path g a b | b `elem` na = [a,b]
           | null subs = []
           | otherwise = a : head subs
  where
    na = neigh g a
    g' = filter noa g where noa (i,j) = i /= a && j /= a
    subs = filter (not.null) [ path g' v b | v <- na ]
