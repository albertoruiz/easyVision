module Initialize where

import qualified Vision.Bootstrap as B

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.LAPACK(rightSVR)
import Util.Misc(Mat,arrayOf,Vec,(&),debug,vec,diagl,replaceAt,intersectSorted,
                 degree,unionSort)
import Util.Estimation(homogSolve,homogSolveG)
import Vision(factorizeCamera,camerasFromEssential,selectCamera',cameraAtOrigin,
              homog,inHomog, triangulate,
              depthOfPoint,depthsOfInducedPoint,qEssen)
import Vision.SparseRep(SparseVP,ako,sKal,lObs,essentials,epiObs,sCam,v_of_p, p_of_v, sPts,commonPoints,  Epi(..))
import Vision.Gea(recompPts)
import Numeric.LinearAlgebra.Tensor
import Numeric.LinearAlgebra.Array.Util
import Data.Maybe(isJust,fromJust)
import Data.List(sortBy,sort,group,foldl',foldl1',maximumBy)
import Data.Function(on)

---------------------------------------------------------------
-- bootstrap3 0.9 30 p --
---------------------------------------------------------------


-- initialization of rotations in a sparse visual problem

rotOfCam :: Mat -> Mat
rotOfCam c = r where (_,r,_) = factorizeCamera c

graphInit t rots = paths
  where paths = ident 3 : map (chain . path t 0) [1.. maximum (map snd t)]
        chain xs = foldl1' (<>) $ map f $ zip xs (tail xs)
        f (i,j) = case lookup (i,j) rots of
                     Just r -> r
                     Nothing -> case lookup (j,i) rots of
                                   Just r -> trans r
                                   Nothing -> error $ "graphInit" ++ show (i,j)


rotRefine w angMax r0 sel = debug "rotation system error gain: " (const err) rs where
    r = arrayOf r0
    f ((i,j),rel) = ((i,j), B.coordsRot $ trans (r j) <> rel <> r i)
    d = debug "rot residuals: " (map (round.h.snd))
    h = (/degree). pnorm PNorm2 . vec
    g = filter ((<angMax).h.snd) . debug "removed: " (map fst . filter ((>=angMax).h.snd))
    resi = d . g $ map f sel
    (sol,err) = B.solveEcs w resi
    rs = zipWith B.fixRot r0 (toLists sol)


solveCams rs sel = (cams1, cams2) where
    cens = fst $ debug "centers system error: " snd $ estimateCenters rs sel
    cams1 = zipWith f rs cens
    cams2 = zipWith f rs (map negate cens)
    f r c = r & asColumn (-r <> c)

selectSol (cams1,cams2) p = if looksRight sol1 then sol1 else sol2 where
    sol1 = recompPts p { sCam = cams1 }
    sol2 = recompPts p { sCam = cams2 }


-- | general parameterizable method
bootstrapGen nspans nlie ndesp p = q where
    nc = length (sCam p)
    sel = filter (isJust.rot.snd) (epiObs p)         -- selected subgraph
    rots = mapSnd (fromJust.rot) sel                 -- extract rotations
    arcs = map fst $ sortBy (compare `on` negate.s2.snd) sel

    spans = take nspans (spanningTrees (nc - 1) arcs)  -- sequence of spanning trees
    r0 = graphInit (head spans) rots
    usedrots = mapSnd (fromJust.rot) $ filter ((`elem` concat spans).fst) sel

    rs = (!!nlie) . iterate f $ r0
        where f r = rotRefine 1 50 r usedrots
    
    desps = map fst $ sortBy (compare `on` negate.h_err.snd) (filter ((>10).nEpi.snd) $ epiObs p)
    spdesp = take ndesp (spanningTrees (nc-1) desps)
    useddesps = filter ((`elem` concat spdesp).fst) (epiObs p)
    
    q = selectSol (solveCams rs useddesps) p

-- | using best two graphs for rots and all pairs for centers
bootstrap0 p = q where
    nc = length (sCam p)
    sel = filter (isJust.rot.snd) (epiObs p)         -- selected subgraph
    rots = mapSnd (fromJust.rot) sel                 -- extract rotations
    arcs1 = map fst $ sortBy (compare `on` negate.s2.snd) sel
    span1 = kruskal (nc - 1) arcs1
    arcs2 = filter (not . (`elem` span1)) arcs1
    span2 = kruskal (nc - 1) arcs2
    r0 = graphInit span1 rots
    r1 = graphInit span2 rots
    rots2graphs = mapSnd (fromJust.rot) $ filter ((`elem` (span1 ++ span2)).fst) sel
    rs = rotRefine 1 50 r0 rots2graphs
    q = selectSol (solveCams rs (epiObs p)) p

-- | using best pairs and all rows
bootstrap3 s2' n' p = q where
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
    q = selectSol (solveCams rs (epiObs p)) p


-- | using best pairs and all rows
bootstrapDummy s2' n' p = q where
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
    q = estimatePointsCenters rs p

-- | with selected points
bootstrapDummy' nsel s2' n' p = q where
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
    q = estimatePointsCenters' nsel rs p


-- | using best pairs and all selected for centers
bootstrap4 s2' n' p = q where
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
    q = selectSol (solveCams rs sel) p

-- | using best pairs and n displacement spans
bootstrap5 s2' n' ndesp p = q where
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

    desps = map fst $ sortBy (compare `on` negate.h_err.snd) (filter ((>10).nEpi.snd) $ epiObs p)
    spdesp = take ndesp (spanningTrees (nc-1) desps)
    useddesps = filter ((`elem` concat spdesp).fst) (epiObs p)
    
    q = selectSol (solveCams rs useddesps) p

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


-- -- returns both possibilities
-- extractRotations :: Mat -> (Mat,Mat)
-- extractRotations esen = (rotOfCam m0, rotOfCam m2) where
--     [m0,_,m2,_] = camerasFromEssential esen


-- returns the good one but may fail
-- extractRotation :: Mat -> Vec -> Vec -> Mat
-- extractRotation esen p p' = r where
--     ms = camerasFromEssential esen
--     m' = selectCamera (f p) (f p') cameraAtOrigin ms
--     r = rotOfCam m'
--     f = toList . inHomog -- we should also admit triangulation of homog points

-- returns Maybe rot
extractRotation' :: Mat -> Vec -> Vec -> Maybe Mat
extractRotation' esen p p' = rotOfCam `fmap` m' where
    ms = camerasFromEssential esen
    m' = selectCamera' (f p) (f p') cameraAtOrigin ms
    f = toList . inHomog

-- relativeRotations :: SparseVP -> [((Int, Int), (Mat, [Int]))] -> [((Int, Int), Mat)]
-- relativeRotations s = map f where
--     f ((i,j), (e, k:_)) = ((i,j), r)
--         where r = extractRotation e p p'
--               p  = ako s (k,i)
--               p' = ako s (k,j)

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
    f ((i,j),rel) = ((i,j), B.coordsRot $ trans (r j) <> rel <> r i)

    d = debug "rot residuals: " (map (round.h.snd))
    h = (/degree). pnorm PNorm2 . vec
    g = filter ((<40).h.snd) . debug "removed: " (map fst . filter ((>=40).h.snd))

-------------------------------------------------------------

{-
--initRots :: [((Int, Int), Mat)] -> ([Mat],[Mat])
initRots p ps = (r0, debug "err rot: " (const err) r)
  where (r0',resi) = residualRots p ps
        (sol,err) = solveEcs 1 resi
        r0 = map r0' [0..maximum (map (snd.fst) ps)]
        r = zipWith fixRot r0 (toLists sol)

--initRots1I :: Mat -> [((Int, Int), Mat)] -> ([Mat],[Mat])
initRots1I p b ps = (f r0, f r)
  where (r0,r) = initRots p ps
        f rs = map (<> (trans (head rs) <> b)) rs

prepareRotsGen q p = initRots1I p (diagl [1,1,1]) . relativeRotations' p . filter q . essentials $ p

-- using only pair with minimum s2
prep s2 = prepareRotsGen (\(_,(e,ks)) -> fst (qEssen e) > s2)
-}

-------------------------------------------------------

estimateCenters rots = p . homogSolve . debug "centers system: " f . dropColumns 3 . coeffCent rots . mapSnd m_hat
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
          f ((i,j),m) = ((i,j), (compact 2 .s.h) (g i j m))
          g i j m = r i!"ai" * t m * r j!"bj"
          t m = (!"kij").listArray[rows m,3,3].toList $ flatten m
          h x = reshape 9 $ coords (x~>"kab")
          s m = m <> trans sel
          sel = (3><9) [ 0,  0, 0, 0, 0, -1,  0, 1, 0,
                         0,  0, 1, 0, 0,  0, -1, 0, 0,
                         0, -1, 0, 1, 0,  0,  0, 0, 0 ]

-- reduced coefficient matrix for a homogeneous linear system
compact nr x = takeRows (rows x `min` nr) c
    where (l,v) = eigSH' (trans x <> x)
          c = diag (sqrt (abs l)) <> trans v

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

neigh g n = unionSort [[ j | (k,j) <- g , k == n ],
                      [ i | (i,k) <- g , k == n ]]



-- spanning tree, from sorted arcs
-- nmax is given from outside to keep laziness
kruskal :: Int -> [(Int, Int)] -> [(Int, Int)]
kruskal nmax s = fst $ myfoldl' ((nmax==).length.fst) f ([],r0) s where
    -- nmax = maximum (map snd s)
    r0 = map return [0..nmax]
    f (g,r) (i,j) = if i `elem` r!!j then (g,r) else ((i,j):g, r')
        where r' = replaceAt z (replicate (length z) z) r
              z = unionSort [r!!i, r!!j]

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

-- | sequence of spanning trees from sorted arcs
spanningTrees nmax arcs = map fst $ tail $ iterate go ([],arcs) where
    go (_,graph) = (t,rest)
        where t = kruskal nmax graph
              rest = filter (not . (`elem` t)) graph

-----------------------------------------------------------

hv = homog . fromList

estimatePointsCenters rots p = q where
    q = p { sPts = newpts, sCam = newCams }
    irk = arrayOf $ zipWith f rots (sKal p) where f r k = trans r <> inv k
    coefs = fromBlocks $ map (return . ec) (lObs p)
    pmax = maximum (map (fst.fst) (lObs p))
    cmax = length (sCam p) - 1
    ec ((i,j),pix) = fromBlocks [ replicate i z
                               ++ [x]
                               ++ replicate (pmax-i+j) z
                               ++ [-x]
                               ++ replicate (cmax-j) z
                                ]
        where [a,b,c] = toList $ irk j <> hv pix
              x = (3><3) [0,-c,b
                          ,c,0,-a
                          ,-b,a,0]
              z = (3><3) [0,0 .. ] :: Mat

    sol = reshape 3 $ fst $ homogSolveG rightSVR $ debug "systemPC:" inforank $ dropColumns 3 coefs where inforank m = (rows m, cols m)
    newpts = (vec [0,0,0,1]:) $ map homog $ toRows $ takeRows (pmax) sol
    newcens = toRows $ dropRows (pmax) sol
    newCams = zipWith f rots newcens where f r c = r & asColumn (-r <> c)

-----------------------------------------------------------------

-- point selection

selectUsefulPoints n p = obs where
    obs = [((r i, j),p) | ((i,j),p) <- lObs p, i `elem` pts ]
    sel = take n . sortBy (compare `on` negate.length.v_of_p p) . p_of_v p   
    pts = unionSort $ map sel [0..length (sCam p) -1]
    r i = fromJust $ lookup i assoc
    assoc = zip pts [0..]

estimatePointsCenters' n rots p = recompPts q where
    q = p { sCam = newCams }
    irk = arrayOf $ zipWith f rots (sKal p) where f r k = trans r <> inv k
    obs = selectUsefulPoints n p
    coefs = fromBlocks $ map (return . ec) obs
    pmax = maximum (map (fst.fst) obs)
    cmax = length (sCam p) - 1
    ec ((i,j),pix) = fromBlocks [ replicate i z
                               ++ [x]
                               ++ replicate (pmax-i+j) z
                               ++ [-x]
                               ++ replicate (cmax-j) z
                                ]
        where [a,b,c] = toList $ irk j <> hv pix
              x = (3><3) [0,-c,b
                          ,c,0,-a
                          ,-b,a,0]
              z = (3><3) [0,0 .. ] :: Mat

    sol = reshape 3 $ fst $ homogSolveG rightSVR $ debug "systemPC:" inforank $ dropColumns 3 coefs where inforank m = (rows m, cols m)
    newcens = toRows $ dropRows (pmax) sol
    newCams = zipWith f rots newcens where f r c = r & asColumn (-r <> c)

