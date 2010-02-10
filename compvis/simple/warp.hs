import EasyVision hiding (warper)
import Numeric.LinearAlgebra
import Vision

szw = Size 600 500

main = do
    sz <- findSize
    (cam,ctrl) <- getCam 0 sz >>= withPause
    prepare

    param <- createParameters [("alpha", realParam (-40) (-100) (100))
                              ,("rho",  realParam 0 (-180) (180))
                              ,("foc",  listParam 2 [0.5, 0.7, 1, 2, 5,5.5, 9,10])
                              ,("sca",  listParam 0.5 [1.1**k|k<-[-20..20]])]

    wCam <- evWindow () "camera" sz Nothing (const (kbdcam ctrl))
    wWarp <-evWindow () "warped" szw Nothing (const (kbdcam ctrl))

    launch (worker wCam wWarp cam param)

-------------------------------------------------------

vector v = fromList v :: Vector Double

warper alpha rho foc sca = r where 
    t = kgen foc
        <> rot1 alpha <> rot3 rho 
        <> kgen (1/foc)
    [a,b] = toList $ inHomog $ t <> vector [0,0,1]
    r = scaling sca <> desp (-a,-b) <> t

----------------------------------------------------------

worker wCam wWarp cam param = do

    camera <- cam >>= return . yuvToRGB
    inWin wCam (drawImage camera)

    alpha <- getParam param "alpha"
    rho   <- getParam param "rho"
    foc   <- getParam param "foc"
    sca   <- getParam param "sca"

    let t = warper (alpha*degree) (rho*degree) foc sca
    inWin wWarp $ do
        drawImage $ warp (80,0,0) szw t camera
