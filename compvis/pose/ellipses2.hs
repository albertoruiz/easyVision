import EasyVision hiding (c1,c2)
import Control.Arrow((***),(&&&))
import Control.Monad(when)
import Data.Colour.Names(red,yellow,orange)
import Graphics.UI.GLUT(lineWidth,($=),clear,ClearBuffer (DepthBuffer),depthFunc,ComparisonFunction (Less),
                        renderPrimitive,PrimitiveMode (Points),vertex,pointSize)
import Util.Ellipses(estimateConicRaw,InfoEllipse(..),analyzeEllipse,conicPoints,tangentEllipses)
import Data.List(sortBy)
import Data.Maybe(isJust)
import Data.Complex(realPart,Complex((:+)),magnitude)
import Util.Misc(Mat,mt,diagl,degree,debug)
import Numeric.LinearAlgebra((<>),ident,trans,inv,toList,takeDiag,fromComplex,toColumns,eig,toRows,fromList,(<.>),complex,NormType (PNorm2),pnorm)
import Util.Homogeneous(adjustRectifier,inHomog,normatdet)
import Vision.Camera(rectifierFromCircularPoint,imagOfCircPt,cameraFromHomogZ0,focalFromCircularPoint,circularConsistency,sepCam)
import Text.Printf(printf)
import Numeric.GSL.Minimization


main = run $ camera ~> grayscale
            >>= wcontours id ~> (id *** contSel)
            >>= contourMonitor "all contours" fst (setColor' red) snd
            ~>  findEllipses
            >>= contourMonitor "detected ellipses" fst (lineWidth $= 3 >> setColor' yellow) snd
            ~>  analyzeEllipses
            >>= contourMonitor "model conics" fst (lineWidth $= 3 >> setColor' orange) (map (Closed . conicPoints 50) . snd)
            ~>  computeRectifier
            >>= showThings
            >>= observe "rectified" (\(im,(_,h)) -> warp zeroP (Size 600 600) h im)
            >>= timeMonitor

----------------------------------------------------------------------

showThings :: IO (ImageGray, ([InfoEllipse], Mat)) -> IO (IO (ImageGray, ([InfoEllipse], Mat)))
showThings c = do
    m <- monitor "misc" (mpSize 10) sh c
    depthFunc $= Just Less
    return m
  where
    sh (img,(es,rec)) = do
        clear [DepthBuffer]
        drawImage' img
        clear [DepthBuffer]
        pointCoordinates (size img)
        when (length es >= 2) $ do
            let [e1,e2] = take 2 es
                [c1,c2] = map conicMatrix [e1,e2]
            setColor' orange
            mapM_ shLine $ map (map realPart) $ tangentEllipses c1 c2

            -- invariant of two conics
            setColor' yellow
            pointSize $= 5
            let vs  = map (fst . fromComplex) . toColumns . snd . eig $ inv c1 <> c2
                vsl = map (fst . fromComplex) . toColumns . snd . eig $ c1 <> inv c2
            _ <- renderPrimitive Points (mapM (vertex . toList. inHomog) vs)
            mapM_ (shLine.toList) vsl



            let okrec = diagl[-1,1,1] <> rec
            let mbcam = cameraFromHomogZ0 Nothing (inv okrec)
                Just cam = mbcam
                elliprec = map (f.conicMatrix) es
                  where f m =  analyzeEllipse $ a <> m <> trans a
                        a = mt okrec
                g InfoEllipse {conicCenter = (x,y), conicSizes = (r,_)} = sphere x y (r/2) (r/2)
            when (isJust mbcam) $ do
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

findEllipses :: (c, [Polyline]) -> (c, [Polyline])
findEllipses = (id *** sortBy (compare `on` negate . area) . filter (isEllipse 5))

analyzeEllipses :: (c, [Polyline]) -> (c, [InfoEllipse])
analyzeEllipses = (id *** map (analyzeEllipse . estimateConicRaw . polyPts))
 
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
rectifierFromCircles es = rectifierFromManyCircles es

rectifierFromManyCircles es@(e1:e2:_) = rec
  where
    mbij = imagOfCircPt e1 e2
    Just ij = mbij
    recraw = rectifierFromCircularPoint (f ij)
    [(mx,my),(mx2,my2)] = map conicCenter [e1,e2]
    rec = adjustRectifier recraw [mx,my] [mx2,my2]
    rectif = if isJust mbij then rec else ident 3 
    f = improveCirc es

improveCirc es (rx:+ix,ry:+iy) = (rx':+ix',ry':+iy') where
    [rx',ix',ry',iy'] = fst $ debug "optim" g $ minimize NMSimplex2 1e-6 600 (replicate 4 0.1) cost [rx,ix,ry,iy]
    cs = map (normatdet.conicMatrix) es
    cost ij  = sum $ map (quality ij) cs
    g = (take 3.toList.head&&&take 3.toList.last).toRows.snd

quality [rx,ix,ry,iy] c = magnitude (p <> complex c <.> p) / np
  where
    p = fromList [rx:+ix,ry:+iy,1]
    np = pnorm PNorm2 p **2

quality' [rx,ix,ry,iy] c = eccentricity (mt t <> c <> inv t)
  where
    t = rectifierFromCircularPoint (rx:+ix,ry:+iy)

    eccentricity con = (d1-d2)/d1 where InfoEllipse {conicSizes = (d1,d2)} = analyzeEllipse con

