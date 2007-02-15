
import EasyVision
import System.Environment(getArgs)
import GSL
import Vision

szc = Size 288 384
szw = Size 600 500

main = do
    args <- getArgs
    let sz = Size 288 384
    (cam,ctrl) <- mplayer (args!!0) szc >>= withPause

    state <- prepare ()

    param <- createParameters state [("alpha", realParam (-40) (-100) (100))
                                     ,("rho",  realParam 0 (-180) (180))
                                     ,("foc",  listParam 2 [0.5, 0.7, 1, 2, 5,5.5, 9,10])
                                     ,("sca",  listParam 0.5 [1.1**k|k<-[-20..20]])]

    addWindow "camera" szc Nothing (const (kbdcam ctrl)) state
    addWindow "warped" szw Nothing (const (kbdcam ctrl)) state

    launch state (worker cam param)

-------------------------------------------------------

vector v = fromList v :: Vector Double

warper alpha rho foc sca = r where 
    t = kgen foc
        <> rot1 alpha <> rot3 rho 
        <> kgen (1/foc)
    [a,b] = toList $ inHomog $ t <> vector [0,0,1]
    r = scaling sca <> desp (-a,-b) <> t

----------------------------------------------------------

worker cam param inWindow st = do

    camera <- cam >>= yuvToGray
    inWindow "camera" (drawImage camera)

    alpha <- getParam param "alpha"
    rho   <- getParam param "rho"
    foc   <- getParam param "foc"
    sca   <- getParam param "sca"

    let t = warper (alpha*degree) (rho*degree) foc sca
    inWindow "warped" $ do
        scale8u32f 0 1 camera >>= warp szw t >>= drawImage

    return st
