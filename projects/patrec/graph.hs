{-# LANGUAGE TupleSections #-}

-- experiments with spectral graph matching

import Classifier.ToyProblems
import Classifier(addNoise,boxAttr,preprocess)
import Util.Misc(debug,vec,Mat,Vec)
import Util.Gaussian(mixturePDF,findMixture)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util(diagl)

import Vision.GUI
import ImagProc

import Graphics.UI.GLUT (vertex,renderPrimitive, PrimitiveMode(Lines))
import Text.Printf(printf)
import Control.Monad(when)
import qualified Util.GP as GP
import Data.List(sort, sortBy)
import Data.Function(on)
import Data.Maybe

import Util.ScatterPlot

scw title p = browser title xs (const id)
  where
    xs = [scatter p (0,1) [] (Draw())]


scw3 name ps = browser3D name xs (const id)
  where
    xs = map (\p-> scatter3D p (0,1,2) [] (Draw())) ps


scwgraph title p g = browser title [d] (const id)
  where
    d = scatter p (0,1) [black] x
    x = color black $ Raw $ do
        renderPrimitive Lines $ mapMatrixWithIndexM_ h g
        indexSample p
    h (i,j) w = when (w>0) (vertex (fst(p!!i)) >> vertex (fst(p!!j)))
    indexSample p = mapM_ (\((v,_),k)-> textAt (Point (v@>0) (v@>1)) (show k)) (zip p [0..])
    
---------------------------------------------------------------------------

main = test sshape 0.1 0.8

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

test prob noise threshold = do
    seed <- return 66666 --  randomIO
    let p = filter ((==snd(head (prob 200))).snd) $ addNoise seed noise $ prob 200
        x = fromRows (map fst p)             -- data matrix
        s = GP.gaussK sigma x x              -- similarity matrix
        g = step (s-threshold)               -- graph (or s)
        --l = lapSym g; u = snd (dbgeig $ eigSH l)    -- laplacian and eigenvectors in columns
        --l = lap g; u = snd (dbgeig $ eigSH l)                   
                           
        --l = lapRW g; u = fst . fromComplex . eigSorted . eig $ l
        (d,l) = lapDL g; u = snd (dbgeig $ geigSH' l d) -- RW
        
        y = fromColumns . take 3 . tail . reverse . toColumns $ u  -- e.g. 2 principal components
        
        y' = takeColumns 3 . snd . eigSH' $ g
    runIt $ do
        scwgraph "graph" p g
        scw "clusters" (map (,"?") (toRows y))
        scw "spectral embedding" (toRows y `zip` map snd p)
        scw3 "spectral embedding" [(ba (toRows y `zip` map snd p))]
        scw3 "naive embedding" [(ba (toRows y' `zip` map snd p))]

ba p = boxAttr p `preprocess` p

dbgeig x = debug "eig" (take 10.reverse.toList.fst) x 
dbgeig' x = debug "eig" (take 10.toList.fst) x
----------------------------------------------------------------------

