import EasyVision
import Data.List
import Data.Function
import Control.Arrow
import Control.Monad
import Vision
import Numeric.LinearAlgebra
import Graphics.UI.GLUT hiding (Size,Point)
import Debug.Trace
import Text.Printf
import Util.Options

f .***. g = uncurry zip . (f *** g) . unzip
f .&&&. g = virtualCamera (map (id &&& id)) >=> virtualCamera (f .***. g)
history n = map (take n) . tails


main = do
    sz <- findSize
    corners <- getCornerDetector
    sc <- getOption "--scale" 0.5
    alpha <- getOption "--alpha" 0.95
    dkey <- getOption "--dkey" 0.3
    mxd <- getOption "--mxd" 0.01
    urad <- getOption "--urad" (-0.28)
    flagurad <- getFlag "--urad"
    undistort <- if not flagurad then return id else flip remap InterpNN `fmap` undistortMap sz 2 urad


    run $   camera ~> float . undistort . grayscale ~~> drop 3
        >>= corners -- >>= cornerMonitor "corners" 
        ~>  toPoints ~~> basicLinks mxd (ident 3,undefined)
        ~~> history 2
        >>= shBasicLinks
        ~>  (\(im,pts,(h,_))->(im,pts,h)) . head
        ~~> toFrame0
--        ~>  warpOne sc >>= drift alpha
        >>= id .&&&. warpAll sc
        >>= zoomWindow "zoom" 600 (toGray.snd)


-----------------------------------------------------

toPoints (im,pixs) = (im, pixelsToPoints (size im) pixs)

basicLinks mxd hPrev ((im1,pts1):rest@((_,pts2):_)) = (im1,pts1,hPrev): basicLinks mxd hNew rest
    where hNew = estimateIt mxd hPrev pts1 pts2

estimateIt mxd (h,_) p1s p2s = (hnew,(g1,g2)) where
    predicted = zip (htp h p2s) p2s
    dist q (p,_) = distPoints p q
    (g1,g2',_,_,_) = basicMatches' (p1s, predicted) dist mxd
    g2 = map snd g2'
    hnew = if length g1 > 4
                then --fst $ estimateHomographyRansac 0.4 0.005 (map pl g1) (map pl g2)
                     estimateHomographyRaw (map pl g1) (map pl g2)
                else ident 3

----------------------------------------------------

warpOne sc (im,pts,h) = warp 1 (Size 600 600) (scaling sc <> h) im

warpAll sc xs = scanl goWarp im0 (tail xs) where
    im0 = warpOne sc (head xs)
    goWarp prev (im,pts,h) = warpOn prev (scaling sc <> h) im 

----------------------------------------------------

toFrame0 = curry3 zip3 . acum . unzip3

acum (iml, ptsl, hs) = (iml, ptsl, scanl1 pronor hs)
    where pronor a b = normatdet (a <> b)

uncurry3 f x y z = f (x,y,z)
curry3 f (x,y,z) = f x y z

--------------------------------------------------

pl (Point x y) = [x,y]
lp [x,y] = Point x y
htp h = map lp . ht h . map pl
htp1 h = lp . head . ht h . return . pl
norm = pnorm PNorm2
fst3 (a,_,_) = a
---------------------------------------------------

sh (img,_,_) = do
    drawImage' img

----------------------------------------------------

shDesp ((img,_,_),p) = do
    drawImage' img
    pointCoordinates (size img)
    lineWidth $= 3
    renderPrimitive Lines $ vertex (Point 0 0) >> vertex p

-----------------------------------------------------

compDesp (img,pts,h) = ((img,pts,h), p)
    where p = htp1 (inv h) (Point 0 0)


close d ih0 (_,_,h) = norm p < d where
    p = inHomog $ (ih0 <> h) <> (3 |> [0, 0, 1::Double])


selectKeyFrames d (x:xs) = x: selectKeyFrames d ys where
    (_,_,h0) = x
    ih0 = inv h0
    ys = dropWhile (close d ih0) xs


markKeyFrames d (x:xs) = Right x : skip d xs where
    (_,_,h0) = x
    ih0 = inv h0
    skip d (a:as) | close d ih0 a = Left a : skip d as
                  | otherwise    = markKeyFrames d (a:as)

mean l = sum l / fromIntegral (length l)

----------------------------------------------------

shBasicLinks = monitor "basicLinks" (mpSize 20) sh where
    sh [(im0, pts0, _),(im1,pts1,(h1,(g1,g2)))] = do
        drawImage' im0
        pointCoordinates (size im0)
        pointSize $= 2; setColor 1 0 0
        renderPrimitive Points $ mapM_ vertex pts0
        pointSize $= 2; setColor 0 0 1
        renderPrimitive Points $ mapM_ vertex (htp h1 pts1)

        pointSize $= 5; setColor 1 1 0
        renderPrimitive Points $ mapM_ vertex g1
        pointSize $= 1; setColor 0 0 0
        renderPrimitive Points $ mapM_ vertex (htp h1 g2)

        setColor 1 0.5 0.5
        text2D 0.9 0.7 $ printf "%.1f" $ 320 * mean (zipWith distPoints g1 (htp h1 g2))
        text2D 0.9 0.6 $ printf "%.1f" $ 320 * mean (zipWith distPoints g1 g2)

--------------------------------------------------------

groupkey l = go l [] where
    go (Right x: ls) prev = (x, added) : go ls added where added = x:prev 
    go (Left x : ls) prev = (x, prev)  : go ls prev

--------------------------------------------------------

findClosest (x,ls) = minimumBy (compare `on` f x) ls where
    f (_,_,h) (_,_,g) = norm p where
        p = inHomog $ (inv h <> g) <> (3 |> [0, 0, 1::Double])
