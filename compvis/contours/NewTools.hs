{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module NewTools (
    injectPrototypes,
    showCanonical, showContours,
    showAlign
) where

import EasyVision
import Control.Arrow((***),(&&&))
import Graphics.UI.GLUT hiding (Point,Size)
import Data.Colour.Names
import Numeric.LinearAlgebra((<>),fromList)
import Text.Printf(printf)
import Util.Misc(diagl,mean)
import Util.Rotation(rot3)
import Vision(desp)
import Classifier(Sample)
import Control.Monad(when)
import Data.List(minimumBy,sortBy)

import Shapes

----------------------------------------------------------------------

injectPrototypes
    :: (Image t, Drawable t)
    => (Polyline -> Polyline)               -- ^ preprocesing function (e.g. boxShape)
    -> IO (Sample Polyline)                 -- ^ how to get the list of prototypes
    -> IO (t, [Shape])                      -- ^ Input: anything + detected shapes
    -> IO (IO ((t, [Shape]), Sample Shape)) -- ^ Output: input + processed prototypes
injectPrototypes prepro ioprots = shapeCatalog fst (map shapeContour.snd) prepro ioprots (map (shape *** id))

showContours = contourMonitor "contours" fst (lineWidth $= 2 >> setColor' red) snd

showCanonical = contourMonitor "canonical" (fst.fst) (lineWidth $= 3 >> setColor' yellow) (map f . snd . fst)
  where
    f Shape {..}  = transPol t (head kShapes)
      where
        t = desp (ox,oy) <> diagl [0.05,0.05,1]
        (ox,oy,_,_,_) = shapeMoments

----------------------------------------------------------------------

showAlign :: IO (ImageGray,[[ShapeMatch]]) -> IO (IO (ImageGray,[[ShapeMatch]]))
showAlign cam = monitorWheel (3,0,3) "Detected" (mpSize 20) sh cam
  where
    labs = concatMap label
    sh k (im, oks) = do
        drawImage' im
        pointCoordinates (size im)
        setColor' green
        let e = (100*) $ mean $ map (invDist.head) $ filter (not.null) oks -- mean classification error x100
            np = mean $ map (fromIntegral.length.polyPts.shapeContour.target.head) $ filter (not.null) oks
        textAt (Point 0.9 0.6) $ printf "N = %d, np = %.0f, E = %.0f" (length oks) (np::Double) e 
        setColor' white
        sc k oks
        mapM_ (h k) $  oks

    h _ [] = return ()
    
    h 0 (ShapeMatch {..} : ps) = textAt (Point ox oy) $ printf "[%d] %s (%.0f) %s"
            (length $ polyPts $ shapeContour target) label (invDist*100) (labs ps)
      where
        Shape { shapeMoments = (ox,oy,_,_,_) } = target

    h 1 (ShapeMatch {..} : ps) = do
        setColor' (color invDist)
        renderPrimitive LineLoop (vertex (bounding (shapeContour target)))
        setColor' yellow
        when (invDist*100 < 20) $ textAt (Point ox oy) (printf "%s" (label++labs ps))
      where
        Shape { shapeMoments = (ox,oy,_,_,_) } = target
        color d | d < 0.1 = blue
                | d < 0.2 = red
                | otherwise = black    

    h 2 (ShapeMatch {..} : _) = do
        setColor' yellow
        renderPrimitive LineLoop (vertex (shapeContour target))
        setColor' red
        renderPrimitive LineLoop (vertex (transPol wa $ shapeContour proto))
        let Shape { shapeMoments = (ox,oy,_,_,_) } = target
        setColor' white; textAt (Point ox oy) (printf "%.f" (100*alignDist))

    h 3 (ShapeMatch {..} : _) = do
        setColor' blue
        renderPrimitive LineLoop (vertex (bounding (shapeContour target)))
        let Shape { shapeMoments = (ox,oy,_,_,_) } = target
        setColor' white; textAt (Point ox oy) label

    sc 0 _ = return ()
    sc 1 _ = return ()
    sc 3 _ = return ()
    sc 2 oks = textAt (Point 0.9 0.5) $ printf "E = %.1f" e
      where
        e = (100*) $ mean $ map (alignDist.head) $ filter (not.null) oks
    
