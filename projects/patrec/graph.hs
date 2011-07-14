{-# LANGUAGE TupleSections #-}

-- experiments with spectral graph matching

import Classifier.ToyProblems
import Classifier(addNoise,boxAttr,preprocess)
import Util.Misc(debug,diagl,vec,Mat,Vec)
import Util.Gaussian(mixturePDF,findMixture)
import Numeric.LinearAlgebra
import EasyVision hiding (whitener)
import Data.Colour.Names as Col
import Graphics.UI.GLUT hiding (Size,scale,Point)
import System.Random(randomIO)
import Text.Printf(printf)
import Control.Monad(when)
import qualified GP
import Data.List(sort, sortBy)

---------------------------------------------------------------------------

colors = [red,blue,orange,green]++repeat Col.lightgray

runIt f = prepare >> f >> mainLoop

scw title p = scatterPlot title (Size 400 400) p (0,1) colors (indexSample p)

scw3 title p = scatterPlot3D title 400 p (0,1,2) colors (return ())

scwgraph title p g = scatterPlot title (Size 400 400) p (0,1) colors f
  where
    f = do
        renderPrimitive Lines $ mapMatrixWithIndexM_ h g
        setColor' black
        indexSample p
    h (i,j) w = when (w>0) (vertex (fst(p!!round i)) >> vertex (fst(p!!round j)))
    

indexSample p = mapM_ (\((v,_),k)-> textAt (Point (v@>0) (v@>1)) (show k)) (zip p [0..])

---------------------------------------------------------------------------

main = test sshape

sigma = 1

lap adj = d - adj
  where
    d = diag $ vec $ map sumElements $ toRows adj

lapSym adj = sd <> (d - adj) <> sd
  where
    vd = vec $ map sumElements $ toRows adj
    d = diag vd
    sd = diag (sqrt . recip $ vd)

lapRW adj = id <> (d-adj)
  where
    vd = vec $ map sumElements $ toRows adj
    d = diag vd
    id = diag . recip $ vd

lapDL adj = (d,(d-adj))
  where
    d = diag $ vec $ map sumElements $ toRows adj

eigSorted (l,v) = fromColumns $ sortWith (negate . magnitude) (toList l) (toColumns v)

sortWith f k v = map snd $ sortBy (compare `on` (f.fst)) $ zip k v

test prob = do
    seed <- return 66666 -- randomIO
    let p = addNoise seed 0.1 $ prob 200
        x = fst (GP.matData p :: (Mat,Vec))  -- data matrix
        s = GP.gaussK sigma x x :: Mat       -- similarity matrix
        g = step (s-0.8)                     -- graph
        --l = lapSym g; u = snd (eigSH l)    -- laplacian and eigenvectors in columns
                           
        --l = lapRW g; u = fst . fromComplex . eigSorted . eig $ l
        (d,l) = lapDL g; u = snd (geigSH' l d) -- RW
        
        y = fromColumns . take 3 . tail . reverse . toColumns $ u  -- e.g. 2 principal components
        
        y' = takeColumns 3 . snd . eigSH' $ g
    runIt $ do
        scwgraph "graph" p g
        --scw "clusters" (map (,"?") (toRows y))
        scw "spectral embedding" (toRows y `zip` map snd p)
        scw3 "spectral embedding" (ba (toRows y `zip` map snd p))
        scw3 "naive embedding" (ba (toRows y' `zip` map snd p))

ba p = boxAttr p `preprocess` p
----------------------------------------------------------------------

