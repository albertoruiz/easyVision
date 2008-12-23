import EasyVision
import Data.List(transpose)
import Graphics.UI.GLUT hiding (RGB,Size,minmax,histogram,Point,set)
import Debug.Trace
import Foreign
import ImagProc.Ipp.Core
import Text.Printf(printf)
import GHC.Float(float2Double)

main = do
    sz <- findSize

    (cam,ctrl) <- getCam 0 sz ~> channels >>= withPause

    prepare


    o <- createParameters [("sigma",realParam 1.2 0 3)
                          ,("sigmaPrev",realParam 1.6 0 3)
                          ,("steps",intParam 3 1 10)
                 --        ,("n1",intParam 3 0 10)
                          ,("n",intParam 1  0 20)
                          ,("h1",realParam 0.04 0 0.1)
                 --         ,("h2",realParam 0.001 0 0.01)
                          ,("rtest",intParam 10 0 100)
                          ,("what",intParam 0 0 6)
                          ,("mode",intParam 0 0 1)
                          ,("test",intParam 0 0 1)
                          ]

    w <- evWindow () "SIFT" sz Nothing  (const (kbdcam ctrl))

    --w2 <- evWindow () "AUX" sz Nothing  (const (kbdcam ctrl))

    launch (worker o cam w)

-----------------------------------------------------------------

worker o cam w = do

    n <- getParam o "n" :: IO Int
    n1 <- getParam o "n1" :: IO Int
    h1 <- getParam o "h1" :: IO Float
--    h2 <- getParam o "h2" :: IO Float
    sigma <- getParam o "sigma" :: IO Float
    sigmaPrev <- getParam o "sigmaPrev" :: IO Float
    steps <- getParam o "steps" :: IO Int
    rtest <- getParam o "rtest" :: IO Int
    what <- getParam o "what" :: IO Int
    mode <- getParam o "mode" :: IO Int
    test <- getParam o "test" :: IO Int

    let sigmas = [sigma*k^n | n <- [0..]] where k = 2**(1/fromIntegral steps)
    --print $ (map (printf "%.1f") $ take 20 sigmas :: [String])


    orig <- cam
    let imr = if test == 1 then blob rtest
                     else (float $ gray orig)
    
        im = gaussS sigmaPrev imr -- resize (Size (2*h) (2*w)) imr where (Size h w) = size imr

    let pyr = pyramid steps sigmaPrev sigma (gaussS sigma im)
        ims = head (head pyr): concatMap f pyr where f l = drop 1 $ take (length l -2) l
        dogs = map dog pyr
        rawpts = concatMap (siftPoints 300 h1) dogs
        pts = zipWith (fixPtsDirect steps sigma) rawpts [1..]

        tk = trunk steps sigmaPrev sigma im
        dtk = dog tk
        ptk = (siftPoints 300 h1) dtk
        ptk' = zipWith (fixPtsDirect steps sigma) ptk [1..] 



    --inWin w2 $ do
     --   drawImage im
        --drawImage (autoscale $ dogs!!1!!n1)
      --  drawImage (pyr!!1!!n1)

    inWin w $ do
        drawImage im

        let shim = case what of
                0 -> im
                1 -> concat pyr!!n                
                2 -> ims!!n
                3 -> tk!!n
                4 -> float $ scale32f8u (-0.1) (0.1) $ dtk!!n
                5 -> float $ scale32f8u (-0.1) (0.1) $ concatMap (take 3) dogs!!n
                6 -> float $ scale32f8u (-0.1) (0.1) $ concat dogs!!n 

        drawImage shim

        pixelCoordinates (size im)
        let desired = if mode == 0 then pts else ptk'
        text2D 40 40 (show $ map length $ take n desired)
        mapM_ box (concat (take n desired))
        
        let ROI r1 r2 c1 c2 = theROI shim
            vert = sampleLine32f shim (Pixel r c1) (Pixel r c2) where r = (r1+r2) `div` 2
        pointCoordinates (size shim)
        renderSignal (map (float2Double.(subtract 0.5)) vert)

------------------------------------------------------------------




dog l = zipWith (|-|) l (tail l)

-- scale space with n images/ octave
trunk n sigmaPrev sigma img = [ gaussS (sigma * k^j) imgp | j<- [0..] ]
    where k = 2 ** (1/fromIntegral n)
          imgp = img --gaussS sigmaPrev img

octave n sigmaPrev sigma img = imgp : rest
    where k = 2 ** (1/fromIntegral n)
          rest = [ gaussS (s j) imgp | j<- [1..n+2] ]
          s j = sigmaRequired sigma (sigma * k^j)
          sigmaRequired sBase sTot = sqrt (sTot^2 - sBase^2)
          imgp = img --gaussS sigmaPrev img

pyramid n sigmaPrev sigma img = o : pyramid n sigmaPrev sigma d
    where o = octave n sigmaPrev sigma img
          d = decimate (o!!n)

siftPoints nmax h dogs = zipWith3 (localMaxScale3 nmax h) dogs (tail dogs) (tail (tail dogs))


decimate img = modifyROI (shrink (1,1)) $ resize32f' InterpLinear sz  img
    where Size h w = roiSize (theROI img)
          sz = Size (h`div`2) (w`div`2)

------------------------------------------------------------------

autoscale im = f im
    where (mn,mx) = minmax im
          f = if mn == mx then scale32f8u 0 1 else scale32f8u mn mx

box (Pixel r c, n) = do
    setColor 1 0 0
    renderPrimitive LineLoop $ do
        vertex (Pixel (r-n) (c-n))
        vertex (Pixel (r-n) (c+n))
        vertex (Pixel (r+n) (c+n))
        vertex (Pixel (r+n) (c-n))
    setColor 1 1 1
    text2D (fromIntegral c) (fromIntegral r) (show n)

fixPts steps sigma ps s = map f ps
    where oct = 2 ^ (s `div` (steps + 1)) 
          k = 2 ** (1/fromIntegral steps)
          f (Pixel r c) = (Pixel (oct*r + 2^(oct)) (oct*c+2^(oct)), round $ sigma*k**fromIntegral s)

fixPtsDirect steps sigma l n = map g l where g x = (x,round $ 1.5*sigma*k^n) where k = 2**(1/fromIntegral steps)


-------------------------------------

blob rad = unsafePerformIO $ do
    img <- image (Size 480 640)
    let vals = [[f r c | c <- [1..640]]| r <- [1..480]]
            where f r c = if (r-240)^2 + (c-320)^2 < rad^2 then 0.7 else 0.2
    setData32f img vals
    return img
