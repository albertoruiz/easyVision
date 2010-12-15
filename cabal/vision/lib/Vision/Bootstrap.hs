module Vision.Bootstrap(
    -- * Rotations
    initRots, refineRots,
    makeEcs,
    solveEcs,
    coordsRot,
    fixRot,
    -- * Centers
    -- unRotEpi, coeffCent,
    solveCams,
    estimatePointsCenters
) where

import Util.Rotation
import Vision.Epipolar
import Util.Misc(vec,Mat,Vec,debug,degree,impossible,(&),arrayOf,
                 unionSort,unitary,replaceAt,round')
import Numeric.LinearAlgebra hiding (i)
import Numeric.LinearAlgebra.Tensor hiding (scalar)
import Numeric.LinearAlgebra.Array.Util hiding (scalar)
import Vision.Types
import Util.Estimation(homogSolve,compact)
import Data.List(sortBy,foldl1')
import Data.Function(on)
import Data.Maybe(fromJust,isJust)
import Util.Homogeneous(Point(..))

----------------------------------------------------------------------

-- best fit of rotations in tangent space

makeEc :: Int -> (Int, Int) -> [Double]
makeEc n (i,j) = replicate i 0 ++ [1] ++ replicate (j-i-1) 0 ++ [-1] ++ replicate (n-j) (0::Double)

makeEcs :: [((Int, Int), [Double])] -> (Mat, Mat)
makeEcs resi = (fromLists coef, fromLists desi)
    where coef = map (makeEc n . fst) resi
          n = maximum $ map (snd.fst) resi
          desi = map snd resi

solveEcs :: Vec -- ^ residual weigths
         -> [((Int,Int),[Double])] -- ^ residuals
         -> (Mat, (Double, Double))
solveEcs w resi = (sol',err)
  where
    (c', d) = makeEcs resi
    c = dropColumns 1 c'
    sol' = fromRows $ vec [0,0,0] : toRows sol
    sol = weight (debug "rotation system size: " f c) `linearSolveSVD` (weight d)
    err = (norm (c <> sol - d), norm d)
    norm x = sqrt (pnorm Frobenius x ** 2 / fromIntegral (rows x)) / degree
    f m = (rows m, cols m, rank m)
    weight m = asColumn w * m

fixRot :: Mat -> [Double] -> Mat
fixRot r [a,b,c] = r <> trans dr
    where --dr = expm (scalar a * g1 |+| scalar b * g2 |+| scalar c * g3 )
          dr = expm (scalar a * rot1g + scalar b * rot2g + scalar c * rot3g)
fixRot _ _ = impossible "fixRot"

coordsRot :: Mat -> [Double]
coordsRot r = [a,b,c] where
    [[_,c,b],
     [_,_,a],
     [_,_,_]] = toLists (logm r)

    logm m = fst . fromComplex . matFunc log . complex $ m


{-
infix 8 ~&~
a ~&~ b = a <> b - b <> a

infixl 6 |+|
a |+| b = a + b + a ~&~ b /2  + (a-b) ~&~ (a ~&~ b) /12
-}

----------------------------------------------------------------------

-- linear estimation of camera centers from known rotations


solveCams :: [Mat] -- ^ Rotations
          -> EpiPairs
          -> (Motion, Motion) -- ^ two solutions
solveCams rs sel = (cams1, cams2) where
    cens = fst $ debug "centers system error" snd $ estimateCenters rs sel
    cams1 = zipWith f rs cens
    cams2 = zipWith f rs (map negate cens)
    f r c = r & asColumn (-r <> c)


estimateCenters :: [Mat] -> EpiPairs -> ([Vec], Double)
estimateCenters rots = p . homogSolve . debug "centers system" f . dropColumns 3 . coeffCent rots . mapSnd m_hat
    where p (s,err) = ((vec[0,0,0]:) . toRows . reshape 3 $ s, err)
          f m = (rows m, cols m, rank m)

mapSnd :: (t1 -> t2) -> [(t, t1)] -> [(t, t2)]
mapSnd f = map (\(a,b)->(a,f b))

-- | create full coefficient matrix
coeffCent :: [Mat] -> [((Int, Int), Mat)] -> Mat
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


unRotEpi :: [Mat] -- ^ Rotations
         -> [((Int, Int), Mat)] -- ^ original M hat 
         -> [((Int, Int), Mat)] -- ^ unrotated and reduced to 2x3
unRotEpi rots = map f
    where r = arrayOf $ map (listArray[3,3].toList.flatten .trans) rots
          f ((i,j),m) = ((i,j), (compact 2 .s.h) (g i j m))
          g i j m = r i!"ai" * t m * r j!"bj"
          t m = (!"kij").listArray[rows m,3,3].toList $ flatten m
          h x = reshape 9 $ coords (x~>"kab")
          s m = m <> trans sel
          sel = (3><9) [ 0,  0, 0, 0, 0, -1,  0, 1, 0,
                         0,  0, 1, 0, 0,  0, -1, 0, 0,
                         0, -1, 0, 1, 0,  0,  0, 0, 0 ]

----------------------------------------------------------------------

selectUsefulPoints :: Int -> Projections Calibrated -> [Proj]
selectUsefulPoints n p = obs where
    obs = [((r i, j),pt) | ((i,j),pt) <- projs p, i `elem` pts ]
    sel = take n . sortBy (compare `on` negate.length.v_of_p p) . p_of_v p   
    pts = unionSort $ map sel (rangeCams p)
    r i = fromJust $ lookup i assoc
    assoc = zip pts [0..]

estimatePointsCenters :: Int -> [Mat] -> Projections Calibrated -> (Motion,Motion)
estimatePointsCenters n rots p = (newCams sol, newCams (-sol)) where
    irk = arrayOf $ map trans rots
    obs = selectUsefulPoints n p
    coefs = fromBlocks $ map (return . ec) obs
    pmax = maximum (map (fst.fst) obs)
    cmax = nCam p - 1
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

    sol = reshape 3 $ fst $ homogSolve $ debug "systemPC" inforank $ dropColumns 3 coefs
      where inforank m = (rows m, cols m)
    newcens s = toRows $ dropRows (pmax) s
    newCams s = zipWith f rots (newcens s) where f r c = r & asColumn (-r <> c)
    hv (Point x y) = unitary (vec [x,y,1])

-----------------------------------------------------------------
-- spanning tree of a graph
-- see also http://stackoverflow.com/questions/4290163/how-can-i-write-a-mst-algorithm-prim-or-kruskal-in-haskell


neigh :: (Ord t) => [(t, t)] -> t -> [t]
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

myfoldl' :: (a -> Bool) -> (a -> t -> a) -> a -> [t] -> a
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
spanningTrees :: Int -> [(Int, Int)] -> [[(Int, Int)]]
spanningTrees nmax arcs = map fst $ tail $ iterate go ([],arcs) where
    go (_,graph) = (t,rest)
        where t = kruskal nmax graph
              rest = filter (not . (`elem` t)) graph

----------------------------------------------------------------------

graphInit :: [(Int, Int)] -> [((Int, Int), Mat)] -> [Mat]
graphInit t rots = paths
  where paths = ident 3 : map (chain . path t 0) [1.. maximum (map snd t)]
        chain xs = foldl1' (<>) $ map f $ zip xs (tail xs)
        f (i,j) = case lookup (i,j) rots of
                     Just r -> r
                     Nothing -> case lookup (j,i) rots of
                                   Just r -> trans r
                                   Nothing -> error $ "graphInit" ++ show (i,j)

-- | Basic initial estimation of rotations from the best s2 spanning tree of the view graph
initRots :: EpiPairs -> [Mat]
initRots epi = r0 where
    nc = maximum (map (snd.fst) epi) + 1
    sel = filter (isJust.rot.snd) epi         -- selected subgraph
    rots = mapSnd (fromJust.rot) sel          -- extract rotations
    arcs1 = map fst $ sortBy (compare `on` negate.s2.snd) sel
    span1 = kruskal (nc - 1) arcs1
    r0 = graphInit span1 rots

lieAverage :: Vec -> Double -> [Mat] -> [((Int,Int),Mat)] -> [Mat]
lieAverage w angMax r0 sel = debug "rotation system error gain: " (const err) rs where
    r = arrayOf r0
    f ((i,j),rel) = ((i,j), coordsRot $ trans (r j) <> rel <> r i)
    d = debug "rot residuals: " (map (round'.h.snd))
    h = (/degree). pnorm PNorm2 . vec
    g = filter ((<angMax).h.snd) . debug "removed: " (map fst . filter ((>=angMax).h.snd))
    resi = d . g $ map f sel
    (sol,err) = solveEcs w resi
    rs = zipWith fixRot r0 (toLists sol)

-- | refine rotations using selected epipolar pairs
refineRots :: (Epi->Bool) -> EpiPairs -> [Mat] -> [Mat]
refineRots prop epi r0 = rs where
    sel = filter (prop.snd)
        . filter (isJust.rot.snd) 
        $ epi
    rotsel = mapSnd (fromJust.rot) sel
    rs = lieAverage 1 50 (lieAverage 1 50 r0 rotsel) rotsel

