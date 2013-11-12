module Util.Graph(
    kruskal,
    neigh,
    path,
    treeGroups,
    connected
) where

import Data.List(sort, group)
import Util.Misc(pairsWith, unionSort, replaceAt)

-- spanning tree of a graph
-- see also http://stackoverflow.com/questions/4290163/how-can-i-write-a-mst-algorithm-prim-or-kruskal-in-haskell


-- | Minimum/maximum spanning tree, from sorted arcs
-- | nmax is given by the caller to keep laziness
kruskal :: Int -> [(Int, Int)] -> [(Int, Int)]
kruskal nmax s = fst $ myfoldl' ((nmax==).length.fst) f ([],r0) s where
    -- nmax = maximum (map snd s)
    r0 = map return [0..nmax]
    f (g,r) (i,j) = if i `elem` r!!j then (g,r) else ((i,j):g, r')
        where r' = replaceAt z (replicate (length z) z) r
              z = unionSort [r!!i, r!!j]


-- TO DO: use scanl and takeWhile
myfoldl' :: (a -> Bool) -> (a -> t -> a) -> a -> [t] -> a
myfoldl' done f z0 xs0 = lgo z0 xs0
    where lgo z _ | done z = z
          lgo z []     = z
          lgo z (x:xs) = let z' = f z x in z' `seq` lgo z' xs    


neigh :: (Ord t) => [(t, t)] -> t -> [t]
neigh g n = unionSort [[ j | (k,j) <- g , k == n ],
                      [ i | (i,k) <- g , k == n ]]


--path from a to b in a tree
path :: [(Int,Int)] -> Int -> Int -> [Int]
path g a b | b `elem` na = [a,b]
           | null subs = []
           | otherwise = a : head subs
  where
    na = neigh g a
    g' = filter noa g where noa (i,j) = i /= a && j /= a
    subs = filter (not.null) [ path g' v b | v <- na ]


treeGroups :: Double -> (a -> a -> Double) -> [a] -> [[a]]
treeGroups th dist xs = gs
  where
    nmx = length xs - 1
    t = kruskal nmx arcs
    arcs = map snd $ filter ((<th).fst) $ sort $ pairsWith f (xs `zip` [0..])
    f (x,i) (y,j) = (dist x y, (i,j))
    con = connected nmx t
    gs = map (map (xs!!)) con


connected :: Int -> [(Int, Int)] -> [[Int]]
connected nmax arcs = fixedPoint grow (map return [0..nmax])
  where
    fixedPoint f x0 = let x1 = f x0 in if x1 == x0 then x0 else fixedPoint f x1
    grow = unique . map (unique . concatMap neigh')
    neigh' x = x : neigh arcs x
    unique :: Ord t => [t] -> [t]
    unique = map head . group . sort

