import Vision.GUI
import Image
import Util.Geometry
import Util.ShCamera ( computeHomography )
import Util.Estimation ( procrustes )
import Util.Homogeneous ( scaling, desp )
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util ((¿), (¦), row, (#) )


main = do
    runIt $ do
        p1 <- clickPoints "model: click points" "--points1" () sh1
        p2 <- clickPoints "target: click points" "--points2" () sh2
        w1 <- standalone (Size 400 400) "projective" ([],[]) [] [] (sh projective)
        w2 <- standalone (Size 400 400) "similar" ([],[]) [] [] (sh similar)
        w3 <- standalone (Size 400 400) "affine" ([],[]) [] [] (sh affine)
        w4 <- standalone (Size 400 400) "simple" ([],[]) [] [] (sh simple)
        mapM_ (connectWith g1 p1) [w1,w2,w3,w4]
        mapM_ (connectWith g2 p2) [w1,w2,w3,w4]

g1 (_,r) (ps,_) = (ps,r)
g2 (l,_) (ps,_) = (l,ps)

sh1 = pointSz 3 . color red . drawPointsLabeled . fst
sh2 = pointSz 3 . color blue . drawPointsLabeled . fst

sh f (xs,ys) = Draw [pointSz 5 $ color red (f xs ys) , pointSz 3 $ color blue ys]


projective :: [Point] -> [Point] -> [Point]
projective xs ys | ok = h ◁ xs
                 | otherwise = []
  where                 
    ok = (min `on` length) xs ys > 3
    h = computeHomography ys xs
    

similar :: [Point] -> [Point] -> [Point]
similar xs ys | ok = h ◁ xs
              | otherwise = []
  where
    n = (min `on` length) xs ys
    ok = n > 1
    [mxs,mys] = map datMat [take n xs, take n ys]
    (s,r,t) = procrustes mys mxs
    rh = fromBlocks [[r,0]
                    ,[0,1]]
    mh = scaling s <> desp (t@>0, t@>1) <> rh
    h = unsafeFromMatrix mh :: Homography


affine :: [Point] -> [Point] -> [Point]
affine xs ys | ok = h ◁ xs
             | otherwise = []
  where
    n = (min `on` length) xs ys
    ok = n > 2
    [mxs,mys] = map datMat [take n xs, take n ys]
    mh = (mxs ¦ 1) <\> mys
    h = unsafeFromMatrix $ trans mh # row [0,0,1] :: Homography


simple :: [Point] -> [Point] -> [Point]
simple xs ys | ok = h ◁ xs
             | otherwise = []
  where
    n = (min `on` length) xs ys
    ok = n > 1
    [mxs,mys] = map datMat [take n xs, take n ys]
    mh' k = flatten $ (mxs¿[k] ¦ 1) <\> mys¿[k]
    u = mh' 0
    v = mh' 1
    mh = (3><3) [ u@>0, 0,    u@>1
                , 0   , v@>0, v@>1
                , 0   , 0   ,  1   ]
    h = unsafeFromMatrix mh :: Homography

