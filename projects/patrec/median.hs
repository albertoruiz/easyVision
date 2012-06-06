{-# LANGUAGE TupleSections #-}

-- experiments with geometric median

import Classifier.ToyProblems
import Classifier(group,addNoise)
import Util.Misc(debug,diagl,vec,Mat,Vec,pairwiseD2)
import Numeric.LinearAlgebra
import Vision.GUI
--import Data.Colour.Names as Col
import Graphics.UI.GLUT hiding (Size,scale,color,windowTitle)
import System.Random(randomIO)
import Text.Printf(printf)
import Control.Monad(when)
import Util.ScatterPlot


----------------------------------------------------------------------

-- Weiszfeld's iteration (http://en.wikipedia.org/wiki/Geometric_median)
improveMedian vs m = m'
  where
    ds = recip $ flatten $ sqrt (pairwiseD2 vs (asRow m))
    ws = ds / scalar (sumElements ds)
    m' = ws <> vs

----------------------------------------------------------------------

scatterPlots name exs mets = browser name xs (const id)
  where
    xs = map f mets
    f (met, name) = scatter exs (0,1) [] (windowTitle name $ drawDecisionRegion 71 exs [] met)


scwmedian title p = browser title [d] (const id)
  where
    d = scatter p (0,1) [black] x
    x = color black $ Raw $ do
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

