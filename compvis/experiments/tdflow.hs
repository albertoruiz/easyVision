import EasyVision
import Control.Arrow
import Graphics.UI.GLUT

camera = findSize >>= getCam 0 ~> channels
observe winname f = monitor winname (mpSize 20) (drawImage.f)
run c = prepare >> (c >>= launch . (>> return ()))

main = do
    sigma <- getOption "--sigma" 3
    thres <- getOption "--thres" 0.1
    satmax <- getOption "--sat" 10
    run    $   camera
           >>= observe "original" rgb
           ~>  edges thres . gaussS sigma . float . gray
           ~>  distanceTransform [1,1.4,2.2] . notI &&& ((float &&& float .times 3 dilate3x3))
           >>= observe "distTrans" (((1/60) .*).fst)
           ~~> comb satmax
           >>= zoomWindow "comb" 600 (toGray.((1/5) .*).fst)
           ~>  restrict . ((id &&& gradients.gaussS 0) *** id)
           >>= observe "gx" (sc.fst.fst)
           >>= observe "gy" (sc.snd.fst)
           >>= monitor "dir" (mpSize 20) shDir

edges th img = canny (gx,gy) (th*2/3,th) where
    gx = (-1) .* sobelVert img
    gy =         sobelHoriz img

comb t ((d1,e1):(d2,e2):rest) = (sat t (d1 |*| snd e2), e2) : comb t ((d2,e2):rest)

sat h = thresholdVal32f h 0 IppCmpGreater

autoscale im = f im
    where (mn,mx) = EasyVision.minmax im
          f = if mn == mx then scale32f8u 0 1 else scale32f8u mn mx

sc = scale32f8u (-20) 20

times n f = (!!n) . iterate f

restrict ((d,g),(e,_)) = ((gx  g|*|e', gy g|*|e'),e)
    where e' = e|*|d

shDir ((gx,gy),e) = do
    drawImage e
    pointCoordinates (mpSize 10)
    let sgx = sum32f gx
        sgy = sum32f gy
        se  = sum32f e
    lineWidth $= 3; setColor 1 1 0
    renderPrimitive Lines $ mapM_ vertex [[0,0],[-sgx/se/20,sgy/se/20]]