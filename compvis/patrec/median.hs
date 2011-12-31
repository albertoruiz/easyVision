{-# LANGUAGE TupleSections #-}

-- experiments with geometric median

import Classifier.ToyProblems
import Classifier(group,addNoise)
import Util.Misc(debug,diagl,vec,Mat,Vec,pairwiseD2)
import Numeric.LinearAlgebra
import EasyVision hiding (whitener)
import Data.Colour.Names as Col
import Graphics.UI.GLUT hiding (Size,scale)
import System.Random(randomIO)
import Text.Printf(printf)
import Control.Monad(when)
import qualified GP

----------------------------------------------------------------------

-- Weiszfeld's iteration (http://en.wikipedia.org/wiki/Geometric_median)
improveMedian vs m = m'
  where
    ds = recip $ flatten $ sqrt (pairwiseD2 vs (asRow m))
    ws = ds / scalar (sumElements ds)
    m' = ws <> vs

----------------------------------------------------------------------

colors = [red,blue,orange,green]++repeat Col.lightgray

scw title p = scatterPlot title (Size 400 400) p (0,1) colors (return ())

scwmedian title p = scatterPlot title (Size 400 400) p (0,1) colors f
  where
    f = do
        let vs = fromRows $ map fst p
            m = fst $ meanCov vs 
            ms = iterate (improveMedian vs) m
        pointSize $= 10
        setColor' blue
        renderPrimitive Points (vertex m)
        pointSize $= 5
        setColor' orange
        renderPrimitive Points (mapM_ vertex (debug "med" last $ take 20 $ tail ms))

----------------------------------------------------------------------

main = test moon

test prob = do
    seed <- randomIO
    let p = addNoise seed 0.1 $ prob 50
        x = head $ fst $ group p
    runIt $ do
        scwmedian "median" (map (,"?") x)
        scwmedian "median with outlier" (map (,"?") (vec[20,-5]:x))
        scwmedian "median with outlier" (map (,"?") (toRows kk))
               
----------------------------------------------------------------------

kk = (5><2) [-1,-1,
             -1, 1,
             -0.5, 0.5,
              2, 0,
              20, -5]

