import EasyVision
import Control.Arrow
import Control.Monad
import Graphics.UI.GLUT hiding (Point)
import ImagProc.Ipp.Core(fval,setData32f)
import Foreign(unsafePerformIO)
import Data.IORef

camera = findSize >>= getCam 0 ~> float . gray . channels
observe winname f = monitor winname (mpSize 20) (drawImage.f)
run c = prepare >> (c >>= launch . (>> return ()))
(.&.) = liftM2 (liftM2 (,))
f .***. g = uncurry zip . (f *** g) . unzip

main = run $   cameraG .&. userParam
           >>= observe "original" fst
           ~>  dist                            &&& snd
           ~~> comb                           .***. id
           ~>  ((id &&& gradients) *** id)     ***  id
           ~>  (restrict &&& snd)              ***  id
           ~>  smooth                          &&& snd
           >>= monitor "flow0" (mpSize 20) shFlow

dist (x,p) =   gaussS (sigma p)
           >>> edges (thresH p) (thresL p)
           >>> sat (satur p) . distanceTransform [1,1.4,2.2] . notI &&& float
            $  x

smooth (x,p) = ((box *** box) *** id) x
    where box =  --filterBox (extent p) (extent p)
                (sigma .*) . gaussS sigma
          sigma = fromIntegral $ extent p

edges thH thL img = canny (gx,gy) (thL,thH) where
    gx = (-1) .* sobelVert img
    gy =         sobelHoriz img

comb ((d1,e1):r@((d2,e2):_)) = (d1,e2) : comb r

sat h = thresholdVal32f h 0 IppCmpGreater

sc = scale32f8u (-20) 20

restrict ((d,g),e) = (gx  g|*|e', gy g|*|e')
    where e' = e|*|d

shFlow (((gx,gy),e0), p) = do
    let s = step p
        q = scaleFlow p
    drawImage (e0)
    setColor 1 1 0
    let ROI r1 r2 c1 c2 = theROI gx
        pos = [Pixel r c| r <- [r1,r1+s .. r2], c <- [c1, c1+s .. c2]]
        vs  = map (\p@(Pixel r c) -> [p, Pixel (r-round(q*fval gy p )) (c + round(q*fval gx p))]) pos
    renderPrimitive Lines $ mapM_ vertex (concat vs)

------------------------------------------------------------------------------------

blob rad desp = unsafePerformIO $ do
    img <- image (EasyVision.Size 480 640)
    let vals = [[f r c | c <- [1..640]]| r <- [1..480]]
            where f r c = if (r-240-desp)^2 + (c-320-desp)^2 <= rad^2 then 0.7 else 0.2
    setData32f img vals
    return img

cameraFun f = do
    vn <- newIORef 0
    return $ do
        n <- readIORef vn
        writeIORef vn (n+1)
        return (f n)

camera' v = cameraFun f where
    f k = blob 50 (v*k `mod`480 -240)

cameraG = do
    t <- getFlag "--test"
    v <- getOption "--speed" 2
    if t then camera' v else camera

------------------------------------------------------------------------------------

data Param = Param { sigma     :: Float
                   , thresH    :: Float
                   , thresL    :: Float
                   , satur     :: Float
                   , step      :: Int
                   , extent   :: Int
                   , scaleFlow :: Float
                   }
userParam = do
    o <- createParameters [("sigma"    ,realParam 3 0 20)
                          ,("thresH"   ,realParam 0.2 0 1)
                          ,("thresL"   ,realParam 0.1 0 1)
                          ,("satur"    ,realParam 10 0 50)
                          ,("step"     ,intParam 10 1 50)
                          ,("extent"  ,intParam 20 1 50)
                          ,("scaleFlow",realParam 2 0 5)]
    return $ return Param `ap` getParam o "sigma"
                          `ap` getParam o "thresH"
                          `ap` getParam o "thresL"
                          `ap` getParam o "satur"
                          `ap` getParam o "step"
                          `ap` getParam o "extent"
                          `ap` getParam o "scaleFlow"
