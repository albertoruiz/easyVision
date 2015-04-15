import Vision.GUI
import Image.Processing
import Util.Options(getRawOption,maybeOption)
import Data.Traversable(traverse)
import Numeric.LinearAlgebra.HMatrix
import Util.Geometry as G
import Util.Camera
import Vision
import Util.Debug(debug,debugMat)
import Data.Maybe(isJust)

main = do
    mbf <- maybeOption "--focal"
    mbimg1 <- getRawOption "--image1" >>= traverse loadRGB
    mbimg2 <- getRawOption "--image2" >>= traverse loadRGB
    runIt $ do
        p1 <- clickPoints "image1: click points" "--points1" (Draw ()) (sh mbimg1)
        p2 <- clickPoints "image2: click points" "--points2" (Draw ()) (sh mbimg2)
        w <- standalone3D (Size 600 600) "stereo geometry" ([],[]) [] [] (drw mbf mbimg1 mbimg2)
        r <- browser "rectified" [] (const id)
        connectWith g1 p1 w
        connectWith g2 p2 w
        connectWith' addepi p1 p2
        connectWith' addepi p2 p1
        connectWith (h mbimg1 mbimg2) w r


g1 (_,r) (ps,_) = (ps,r)
g2 (l,_) (ps,_) = (l,ps)

addepi (p2,_) (p1,_) = (p2, Draw l)
  where
    p2l (Point x y) = [x,y]
    pts = map p2l p2
    pts' = map p2l p1
    f = estimateFundamental pts' pts
    l = unsafeFromVector $ tr f #> pt2hv (last p1) :: HLine


sh mbimg (pts,l) = Draw [ Draw $ fmap (rgb.channelsFromRGB) mbimg
                    , color lightgreen . drawPointsLabeled $ pts 
                    , l ]


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
    
    dr2 = Draw [ color red $ showCamera szc im mbimg1'
               , color blue $ showCamera szc im' mbimg2'
               , color black . pointSz 2 $ x3d
               --, color black . pointSz 2 $ drawPoints3DLabeled x3d
               , pointSz 5 . color red $ toImagePlane im szc [epi1]
               , pointSz 5 . color blue $ toImagePlane im' szc [epi2]
               -- , color green $ G.join (cenCam im) (cenCam im')
               , color green $ lineStrip [cen1, cen2]
               , color pink $ lineStrip [cen1, G.homog $ last x3d,cen2]
               , color lightgray [rays1, rays2]
               ]
    
    ok = length pts > 7 && length pts' > 7 && isJust mbm'
    
    dr = if ok then dr2 else Draw ()
    
    mbimg1' = tf mbimg1
    mbimg2' = tf mbimg2
    tf = fmap (toFloat.grayscale.channelsFromRGB)
    
    
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
    
    
    epi1 = unsafeFromVector $ inHomog $ null1 f
    epi2 = unsafeFromVector $ inHomog $ null1 $ tr f

    info = debugMat "F" 6 (const $ normat3 f)
         . debug "f" (const df) 
         . debug "q" (const q)
         . debug "bougnoux" (const (bougnoux f))

move d m = k <> (r ¦ asColumn (-r #> (scalar d * c)))
  where
    (k,r,c) = factorizeCamera m

--------------------------------------------------------------------------------

h (Just x1) (Just x2) (k,_) ps | not (ok ps) = (k, [ Draw $ blockImage[[x1,x2]] ])
  where
    ok (psl,psr) = length (zip psl psr) >= 8

h (Just x1) (Just x2) (k,_) (psl,psr) = (k, [ Draw $ blockImage[[y1,y2]]
                                            , Draw $ y12 ])
  where
    p2l (Point x y) = [x,y]
    pts = map p2l psl
    pts' = map p2l psr
    f = estimateFundamental pts' pts
    (ep,ep') = epipoles f
    (h, h') = stereoRectifiers f pts pts'
    sz = Size 800 600
    y1 = warp (Word24 80 0 0) sz h x1
    y2 = warp (Word24 80 0 0) sz h' x2
    y12 = 0.5 .* g y1 |+| 0.5 .* g y2
    g = toFloat . grayscale . channelsFromRGB
h _ _ (k,_) _ = (k,[])

