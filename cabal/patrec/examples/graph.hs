{-# LANGUAGE TupleSections #-}

-- experiments with spectral graph matching

import Classifier.ToyProblems
import Classifier(addNoise)
import Util.Misc(debug,diagl,vec,Mat,Vec)
import Util.Gaussian(mixturePDF,findMixture)
import Numeric.LinearAlgebra
import EasyVision hiding (whitener)
import Data.Colour.Names as Col
import Graphics.UI.GLUT hiding (Size,scale)
import System.Random(randomIO)
import Text.Printf(printf)
import Control.Monad(when)
import qualified GP

---------------------------------------------------------------------------

colors = [red,blue,orange,green]++repeat Col.lightgray

runIt f = prepare >> f >> mainLoop

scw title p = scatterPlot title (Size 400 400) p (0,1) colors (return ())

scwgraph title p g = scatterPlot title (Size 400 400) p (0,1) colors f
  where
    f = do
        renderPrimitive Lines $ mapMatrixWithIndexM_ h g
    h (i,j) w = when (w>0) (vertex (fst(p!!round i)) >> vertex (fst(p!!round j)))

---------------------------------------------------------------------------

main = test sshape

sigma = 1

lap adj = (d - adj)
  where
    d = diag $ vec $ map sumElements $ toRows adj

test prob = do
    seed <- randomIO
    let p = addNoise seed 0.1 $ prob 200
        x = fst (GP.matData p :: (Mat,Vec))  -- data matrix
        s = GP.gaussK sigma x x :: Mat       -- similarity matrix
        g = step (s-0.8)                     -- graph
        l = lap g :: Mat                     -- laplacian
        u = snd (eigSH' l)                   -- eigenvectors in columns
        y = dropColumns (cols u - 2) u  -- e.g. 2 principal components
    print $ eigenvaluesSH' l    
    --print $ last $ toColumns $ snd $ eigSH l
    --mapM_ print q
    print y
    runIt $ do
        scwgraph "graph" p g
        scw "clusters" (map (,"?") (toRows y))

