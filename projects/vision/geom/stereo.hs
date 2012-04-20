import Vision.GUI
import ImagProc hiding (Pixel(..))
import Util.Options(getRawOption,maybeOption)
import Data.Traversable(traverse)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util((!))
import Util.Geometry as G
import Util.Camera
import Vision
import Util.Misc(debug,debugMat)
import Data.Maybe(isJust)

main = do
    mbf <- maybeOption "--focal"
    mbimg1 <- getRawOption "--image1" >>= traverse loadRGB
    mbimg2 <- getRawOption "--image2" >>= traverse loadRGB
    runIt $ do
        p1 <- clickPoints' "image1: click points" "--points1" (sh mbimg1)
        p2 <- clickPoints' "image2: click points" "--points2" (sh mbimg2)
        w <- standalone3D (Size 600 600) "stereo geometry" ([],[]) [] [] (drw mbf mbimg1 mbimg2) 
        connectWith g1 p1 w
        connectWith g2 p2 w

sh mbimg pts = Draw [ Draw $ fmap (rgb.channelsFromRGB) mbimg
                    , color lightgreen . drawPointsLabeled $ pts 
                    ]

g1 (_,r) ps = (ps,r)
g2 (l,_) ps = (l,ps)

drw mbf mbimg1 mbimg2 (psl,psr) = clearColor white [ color gray $ axes3D 2 , dr ]
  where
    p2l (Point x y) = [x,y]
    pts = map p2l psl
    pts' = map p2l psr
    f = estimateFundamental pts' pts
    (df,e) = case mbf of
        Just uf -> (uf, (kgen uf <> f <> kgen uf))
        Nothing -> (edf, ee)
    (ee,edf,err) = estimateEssential 2 f
    q = qualityOfEssential (kgen df <> f <> kgen df)
    m = kgen df <> cameraAtOrigin
    ms = map (kgen df <>) (camerasFromEssential e)
    mbm' = selectCamera' (head pts) (head pts') m ms
    Just m'1 = mbm'
    m' = move 5 m'1

    szc = 1

    x = triangulate [(m, pts), (m', pts')]
    x3d = info $ map (\[x,y,z]->Point3D x y z) x
    
    dr2 = Draw [ color red $ showCam szc im mbimg1
               , color blue $ showCam szc im' mbimg2
               , color black . pointSz 2 $ x3d
               --, color black . pointSz 2 $ drawPoints3DLabeled x3d
               , pointSz 5 . color red $ toImagePlane im szc [epi1]
               , pointSz 5 . color blue $ toImagePlane im' szc [epi2]
               -- , color green $ G.join (cenCam im) (cenCam im')
               , color green $ lineStrip [cen1, cen2]
      --         , color pink $ lineStrip [cen1, G.homog $ last x3d,cen2]
               , color lightgray [rays1, rays2]
               ]
    
    ok = length pts > 7 && length pts' > 7 && isJust mbm'
    
    dr = if ok then dr2 else Draw ()
    
    im = infoCam $ unsafeFromMatrix m
    im' = infoCam $ unsafeFromMatrix m'
    
    cen1 = cenCam im
    cen2 = cenCam im'
    
    (sp1,sp2) = last $ zip psl psr
    iptsl = toImagePlane im szc [sp1]
    iptsr = toImagePlane im' szc [sp2]
    
    invc1 = invTrans (cam im)
    rays1 = invc1 <| G.homog sp1
    invc2 = invTrans (cam im')
    rays2 = invc2 <| G.homog sp2
    
    
    epi1 = unsafeFromVector $ inHomog $ nullVector f
    epi2 = unsafeFromVector $ inHomog $ nullVector $ trans f

    
    info = debugMat "F" 6 (const $ normat3 f)
         . debug "f" (const df) 
         . debug "q" (const q)
         . debug "bougnoux" (const (bougnoux f))

move d m = k <> (r ! asColumn (-r <> (scalar d * c)))
  where
    (k,r,c) = factorizeCamera m

