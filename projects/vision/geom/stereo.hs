import Vision.GUI
import ImagProc hiding (Pixel(..))
import Util.Options(getRawOption)
import Data.Traversable(traverse)
import Numeric.LinearAlgebra
import Util.Geometry
import Util.Camera
import Vision
import Util.Misc(debug,debugMat)
import Data.Maybe(isJust)

main = do
    mbimg1 <- getRawOption "--image1" >>= traverse loadRGB
    mbimg2 <- getRawOption "--image2" >>= traverse loadRGB
    runIt $ do
        p1 <- clickPoints' "image1: click points" "--points1" (sh mbimg1)
        p2 <- clickPoints' "image2: click points" "--points2" (sh mbimg2)
        w <- standalone3D (Size 600 600) "stereo geometry" ([],[]) [] [] (drw mbimg1 mbimg2) 
        connectWith g1 p1 w
        connectWith g2 p2 w

sh mbimg pts = Draw [ Draw $ fmap (rgb.channelsFromRGB) mbimg
                    , color lightgreen . drawPointsLabeled $ pts 
                    ]

g1 (_,r) ps = (ps,r)
g2 (l,_) ps = (l,ps)

drw mbimg1 mbimg2 (psl,psr) = clearColor white [ color gray $ axes3D 2 , dr ]
  where
    p2l (Point x y) = [x,y]
    pts = map p2l psl
    pts' = map p2l psr
    f = estimateFundamental pts' pts
    (e,df,err) = estimateEssential 2 f
    --df = 1.7
    --e = (kgen df <> f <> kgen df)
    q = qualityOfEssential (kgen df <> f <> kgen df)
    m = kgen df <> cameraAtOrigin
    ms = map (kgen df <>) (camerasFromEssential e)
    mbm' = selectCamera' (head pts) (head pts') m ms
    Just m' = mbm'
    x = triangulate [(m, pts), (m', pts')]
    x3d = info $ map (\[x,y,z]->Point3D x y z) x
    
    dr2 = Draw [ color red $ showCam 0.2 (infoCam $ unsafeFromMatrix m) mbimg1
               , color blue $ showCam 0.2 (infoCam $ unsafeFromMatrix m') mbimg2
               , color black . pointSz 2 $ x3d
               --, color black . pointSz 3 $ drawPoints3DLabeled x3d
               ]
    
    ok = length pts > 7 && length pts' > 7 && isJust mbm'
    
    dr = if ok then dr2 else Draw ()
    
    info = debugMat "F" 6 (const f)
         . debug "f" (const df) 
         . debug "q" (const q)
         . debug "bougnoux" (const (bougnoux f))

