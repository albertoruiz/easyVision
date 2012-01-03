import EasyVision as EV
import Util.Misc(Mat,vec,debug,debugMat,diagl)
import Numeric.LinearAlgebra
import Graphics.UI.GLUT
import Data.Colour.Names as Col
import Control.Arrow((&&&),(***))
import Control.Applicative((<$>))
import Data.List(sort,sortBy)
import Util.Rotation(rot3)
import Vision

disp = putStr . dispf 3

main = do
    let prob = "temple"
        path = "SparseRing/" -- path = "/" --path = "Ring/"
        pre = "SR_" --pre = "_" --pre = "R_"
    f <- readFile ("../../data/dense/"++prob++path++prob++pre++"par.txt")
    let cs = map (readCam . map read . tail . words) . sort $ tail (lines f)
    print (length cs)
    ims <- sortBy (compare `on` snd) <$> readFolder' ("../../data/dense/"++prob++path)
    let cis = zip cs (map fst ims)
    runIt $ do
        shReco cis
        -- imagesBrowser "dino" (mpSize 10) ims
        shEpi cis

k0 = (3><3) [ 1,   0,   -320,
              0,    1,  -240,
              0,    0,   320] :: Mat

readCam :: [Double] -> Mat
readCam [k11, k12, k13,
         k21, k22, k23, 
         k31, k32, k33,
         r11, r12, r13,
         r21, r22, r23,
         r31, r32, r33,
         t1,  t2,  t3] =  diagl [1,1,-1] <> k0 <> k <> rt
  where
    k =  (3><3) [k11, k12, k13,
                 k21, k22, k23,
                 k31, k32, k33]
    rt = (3><4) [r11, r12, r13, t1,
                 r21, r22, r23, t2,
                 r31, r32, r33, t3]

shReco cis = do
    let cts = map (id *** extractSquare 128 . float . grayscale. channelsFromRGB) cis
    evWin3D cts "Reco" 500 (Just $ disp cts) (const kbdQuit)
    clearColor $= Color4 1 1 1 1
  where
    disp cts _ = do
        setColor' red
        let drawCam (c,t) = drawCamera 0.03 c (Just t)
        mapM_ drawCam cts
        setColor' blue
        sequence_ $ zipWith indexCam (map fst cts) [0..]
        drawAxes3D
        
indexCam c s = rasterPos (Vertex3 x y z) >> renderString Helvetica12 (show s)
  where (_,_,cen) = factorizeCamera c
        [x,y,z] = toList cen


shEpi cis = examplesBrowser "Epi" (mpSize 20) sh (zip cisep (map show [0..]))
  where
    cisep = map f cis
    c0 = fst (head cis)
    p = vec [0.51,-0.44,1]
    -- p = vec [-0.34,-0.45,1]
    f (cam,img) = (img, l)
      where
        f = fundamentalFromCameras c0 cam
        l = f <> p
    sh (im,l) = do
      drawImage im
      pointCoordinates (size im)
      setColor' Col.gray; renderAxes
      setColor' red; shLine (toList l)
      setColor' blue; pointSize $= 3; renderPrimitive Points (vertex (toList p))


drawAxes3D = do
    setColor' Col.gray
    renderPrimitive Lines $ mapM_ vertex $ [[0::Double,0,0],[1,0,0],[0,0,0],[0,1,0],[0,0,0],[0,0,1]]
    setColor' black
    rasterPos (Vertex3 1 0 (0::Double)); renderString  Helvetica12 "x"
    rasterPos (Vertex3 0 1 (0::Double)); renderString  Helvetica12 "y"
    rasterPos (Vertex3 0 0 (1::Double)); renderString  Helvetica12 "z"

