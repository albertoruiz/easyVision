import EasyVision hiding (c1,c2)
import Control.Arrow((***),(&&&))
import Data.Traversable(traverse)
import Control.Monad(when)
import Data.Colour.Names(red,yellow,orange,purple)
import Graphics.UI.GLUT hiding (Point, Size)
import Util.Ellipses
import Data.List(sortBy)
import Data.Maybe(isJust)
import Util.Misc(Mat,mt,diagl,degree)
import Numeric.LinearAlgebra
import Vision
import Text.Printf(printf)


main = run $ camera ~> grayscale
         >>= wcontours id ~> (id *** contSel)
         >>= showContours
         ~>  (modelEllipses . findEllipses)
         >>= showConics
         ~>  computeRectifier
         >>= observe "rectified" (\(im,(_,h)) -> warp zeroP (Size 600 600) h im)
         >>= showThings
         >>= showPose
         >>= timeMonitor

----------------------------------------------------------------------

findEllipses :: (c, [Polyline]) -> (c, [Polyline])
findEllipses = (id *** sortBy (compare `on` negate . area) . filter (isEllipse 5))

modelEllipses :: (c, [Polyline]) -> (c, [InfoEllipse])
modelEllipses = (id *** map (analyzeEllipse . estimateConicRaw . polyPts))
 
computeRectifier :: (c, [InfoEllipse]) -> (c, ([InfoEllipse],Mat))
computeRectifier = (id *** (id &&& rectifierFromCircles) )    

----------------------------------------------------------------------

rectifierFromCircles :: [InfoEllipse] -> Mat

rectifierFromCircles [] = ident 3
rectifierFromCircles [_] = ident 3

rectifierFromCircles [e1,e2] = rectif
  where
    mbij = imagOfCircPt e1 e2
    Just ij = mbij
    recraw = rectifierFromCircularPoint ij
    [(mx,my),(mx2,my2)] = map conicCenter [e1,e2]
    rec = adjustRectifier recraw [mx,my] [mx2,my2]
    rectif = if isJust mbij then rec else ident 3

-- provisional
rectifierFromCircles es = rectifierFromCircles (take 2 es)

----------------------------------------------------------------------

adjustRectifier' r p1 p2 = s <> r
  where
    [p1',p2'] = ht r [p1,p2]
    s = similarFrom2Points p1' p2' [0,0] [0.5,0]

----------------------------------------------------------------------

showContours = contourMonitor "contours" fst (lineWidth $= 3 >> setColor' red) snd

showConics = contourMonitor "model conics" fst (lineWidth $= 3 >> setColor' yellow) f
  where f = map (Closed . conicPoints 50) . snd

----------------------------------------------------------------------

showThings :: IO (ImageGray, ([InfoEllipse], Mat)) -> IO (IO (ImageGray, ([InfoEllipse], Mat)))
showThings c = do
    m <- monitorWheel (0,0,3) "misc" (mpSize 10) sh c
    depthFunc $= Just Less
    return m
  where
    sh k (img,(es,rec)) = do
        clear [DepthBuffer]
        drawImage' img
        clear [DepthBuffer]
        pointCoordinates (size img)
        when (length es >= 2) $ do
            let [e1,e2] = take 2 es
                [c1,c2] = map conicMatrix [e1,e2]
            
            when (k `elem` [0,1]) $ do
                setColor' orange
                mapM_ shLine $ map (map realPart) $ tangentEllipses c1 c2

                -- invariant of two conics
                setColor' yellow
                pointSize $= 5
                let vs  = map (fst . fromComplex) . toColumns . snd . eig $ inv c1 <> c2
                    vsl = map (fst . fromComplex) . toColumns . snd . eig $ c1 <> inv c2
                renderPrimitive Points (mapM_ (vertex . toList. inHomog) vs)
                mapM_ (shLine.toList) vsl

            when (k `elem` [0,2]) $ do
                -- intersections and horizon
                
                let [m1,m2] = map conicCenter [e1,e2]
                    [c1,c2] = map conicMatrix [e1,e2]
                    (Just ij, Just other) = selectSol m1 m2 (intersectionEllipses c1 c2)
                    [htrue,hfalse] = getHorizs [ij,other]
                setColor' purple
                pointSize $= 5
                renderPrimitive Points $ mapM_ (\(a,b) -> vertex (Point (realPart a) (realPart b))) [ij,other]
                mapM_ shLine [htrue,hfalse]

            let okrec = diagl[-1,1,1] <> rec
            let mbcam = cameraFromHomogZ0 Nothing (inv okrec)
                Just cam = mbcam
                elliprec = map (f.conicMatrix) es
                  where f m =  analyzeEllipse $ a <> m <> trans a
                        a = mt okrec
                g InfoEllipse {conicCenter = (x,y), conicSizes = (r,_)} = sphere x y (r/2) (r/2)
            when (k `elem` [0,3] && isJust mbcam) $ do
                let Just ij = imagOfCircPt e1 e2
                    [f1',f2',_] = toList . takeDiag . fst . sepCam $ cam
                setColor' red
                text2D 0.9 0.6 $ printf "f = %.2f (%.2f, %.2f)" (focalFromCircularPoint ij) f1' f2'
                text2D 0.9 0.6 $ printf "f = %.2f" $ focalFromCircularPoint ij
                text2D 0.9 0.5 $ printf "ang = %.1f" $ abs ((acos $ circularConsistency ij)/degree - 90)
                clear [DepthBuffer]
                cameraView cam (4/3) 0.1 100
                mapM_ g elliprec

----------------------------------------------------------------------

showPose :: IO (ImageGray, ([InfoEllipse], Mat)) -> IO (IO (ImageGray, ([InfoEllipse], Mat)))
showPose = monitor3D "pose" 400 sh
  where
    sh (im,(_,rec)) = do
        let okrec = diagl[-1,1,1] <>rec
            fim = float im
            floor = warp 1 (Size 256 256) okrec fim
        drawTexture floor $ map (++[-0.01]) [[1,1],[-1,1],[-1,-1],[1,-1]]
        let shCam = flip (drawCamera 0.2) (Just (extractSquare 128 fim))
            mbcam = cameraFromHomogZ0 Nothing (inv okrec)
        setColor' orange
        traverse shCam mbcam
        return ()

