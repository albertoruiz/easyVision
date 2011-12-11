import EasyVision
import Control.Arrow((***),(&&&))
import Graphics.UI.GLUT hiding (Point,Size)
--import qualified Graphics.UI.GLUT as GL
import Data.Colour.Names
import Numeric.LinearAlgebra
import Text.Printf(printf)
import Data.List(minimumBy,sortBy,groupBy)
import Util.Misc(Mat,Vec,norm,degree,diagl,debug,posMax)
import Util.Rotation
import Classifier(Sample)
import Vision
import Util.Options(optionFromFile)

import Control.Monad(when)
import Control.Applicative((<$>))
import Data.Maybe(isJust)

import ImagProc.C.NP

import Contours
import NewTools

square = Closed $ map (\(a,b)->Point a b) [(0, 0), (0, 0.25), (0, 0.5), (0, 0.75), (0,1), (0.25, 1), (0.5, 1), (0.75, 1), (1, 1), (1, 0.75), (1, 0.5), (1, 0.25), (1, 0), (0.75, 0), (0.5, 0), (0.25,0)]

catalog = (read <$> readFile "../../data/shapes/digits.txt") >>= optionFromFile "--catalog"


main = main1

main1 = run $ camera  ~> grayscale
--       >>= detectStatic 0.02 1.5 5 grayscale rgb ~> grayscale
--         >>= wcontours id ~> (id *** contSel)
         >>= wnpcontours ~> (id *** filter isBlack)
         ~>  id *** filter (not . elongated 8) . map shape
         >>= injectPrototypes boxShape catalog
         >>= showCanonical
         >>= showDirs
         ~>  matchShapes 0.3 0.25               -- TODO: parameter window
--         ~>  matchShapesSimple 0.3
         >>= showAlign
         >>= timeMonitor


----------------------------------------------------------------------

isBlack x = orientedArea x < 0

----------------------------------------------------------------------

runIt f = prepare >> f >> mainLoop

showDigits = runIt $ do
    --ds <- return pentominos
    ds <- (read <$> readFile "../../data/shapes/letters.txt")
    examplesBrowser "digits" (Size 500 500) f (g ds)
  where
    f = shcont . transPol (diagl[0.1,0.1,1])
    g = concatMap (\(x,c)-> zip (kShapes (shape x)) (map (c++) $ map show [1::Int ..]))


showShapes = runIt $ do
    ds <- (read <$> readFile "../../data/shapes/letters.txt")
    examplesBrowser "Shapes" (Size 200 1000) (f.rev) ds
    clearColor $= Color4 1 1 1 1
  where
    f c = do
        let s = 0.03
            prots = reverse $ kShapes (shape c)
            g d = shcont . transPol (desp (d/5,0) <> diagl[s,s,1])
            x = transPol (diagl[3,3,1]) (boxShape c)
            h d c = do textAt (Point (d/5) (-4*s)) (printf "%.1f" (kurtosisX  c))
                       textAt (Point (d/5) (-5*s)) (printf "%.1f" (skewX $ transPol (rot3 0)  c))
        setColor' red
        g 4 x 
        setColor' black
        sequence_ $ zipWith g [3,2 .. ] prots
        sequence_ $ zipWith h [3,2 .. ] prots

