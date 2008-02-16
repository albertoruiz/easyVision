import EasyVision
import Data.List(transpose)
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set)
import Debug.Trace

main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz >>= withChannels >>= withPause

    prepare


    o <- createParameters [("sigma",realParam 1.2 0 3)
                          ,("steps",intParam 3 1 10)
                          ,("n1",intParam 3 0 10)
                          ,("n",intParam 1  0 15)
                          ,("h",realParam 0.04 0 0.1)
                          ]

    w <- evWindow () "SIFT" sz Nothing  (const (kbdcam ctrl))

    --w2 <- evWindow () "AUX" sz Nothing  (const (kbdcam ctrl))

    launch (worker o cam w)

-----------------------------------------------------------------

worker o cam w = do

    n <- getParam o "n" :: IO Int
    n1 <- getParam o "n1" :: IO Int
    h <- getParam o "h" :: IO Float
    sigma <- getParam o "sigma" :: IO Float
    steps <- getParam o "steps" :: IO Int
    orig <- cam

    let im = (float $ gray orig)
    let pyr = take 4 $ pyramid steps sigma im
        dogs = map dog pyr
        rawpts = take n $ concatMap (siftPoints 300 h) dogs

    let pts = zipWith (fixPts steps sigma) rawpts [0..]

    --inWin w2 $ do
     --   drawImage im
        --drawImage (autoscale $ dogs!!1!!n1)
      --  drawImage (pyr!!1!!n1)

    inWin w $ do
        drawImage im
        --drawImage (concat pyr!!n)
        drawImage (autoscale $ concat dogs!!n1)
        --drawImage (autoscale $ p!!n)
        pixelCoordinates (size im)
        text2D 40 40 (show $ map length $ take n rawpts)
        setColor 1 0 0
        --let boxes k = mapM_ (box (2*k)) (pts!!k)
        mapM_ box (concat pts)

------------------------------------------------------------------




dog l = zipWith (|-|) l (tail l)

octave n sigma img = img : [ gaussS (sigma * k^j) img | j<- [0..n+2] ]
    where k = 2 ** (1/fromIntegral n)

gaussS s = filter32f mask . filter32f (transpose mask)
    where mask = nor [map (fg s) [-k .. k]] where k = fromIntegral (ceiling (2*s))
          fg s x = exp (-0.5* (x/s)^2)
          nor m = map (map (/s)) m where s = sum (concat m)

pyramid n sigma img = o : pyramid n sigma (decimate (o!!n))
    where o = octave n sigma img
          d = decimate (o!!n)

siftPoints nmax h dogs = zipWith3 (localMaxScale3 nmax h) dogs (tail dogs) (tail (tail dogs))


decimate img = modifyROI (shrink (1,1)) $ resize32f' sz  img
    where Size h w = roiSize (theROI img)
          sz = Size (h`div`2) (w`div`2)

------------------------------------------------------------------

autoscale im = f im
    where (mn,mx) = minmax im
          f = if mn == mx then scale32f8u 0 1 else scale32f8u mn mx

box (Pixel r c, n) = renderPrimitive LineLoop $ do
    vertex (Pixel (r-n) (c-n))
    vertex (Pixel (r-n) (c+n))
    vertex (Pixel (r+n) (c+n))
    vertex (Pixel (r+n) (c-n))

fixPts steps sigma ps s = map f ps
    where oct = 2 ^ (s `div` (steps + 1))
          k = 2 ** (1/fromIntegral steps)
          f (Pixel r c) = (Pixel (oct*r) (oct*c), round $ sigma*k**fromIntegral s)
