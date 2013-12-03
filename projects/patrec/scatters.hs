import Classifier
import Classifier.ToyProblems
import Util.Stat
import Classifier.Regression(msError)
import Util.ICA
import Util.Misc(debug,randomPermutation)
import Util.Gaussian(mixturePDF,findMixture)

import Numeric.LinearAlgebra
import Vision.GUI.Simple
import Data.Maybe(maybe)
import System.Random(randomIO)
import Text.Printf(printf)
import Control.Monad((>=>))
import Image(Size(..),mat2img)

import Util.ScatterPlot

---------------------------------------------------------------------------

rawmnist = loadExamples "../../data/ml/mnist.txt"

main = do
    scatters (map return "0123")
    
---------------------------------------------------------------------------

scw3 name ps = browser3D name xs (const id)
  where
    xs = map (\p-> scatter3D p (0,1,2) [] (Draw())) ps

---------------------------------------------------------

scatters s = runIt $ do
    raw <- rawmnist
    let sel = filter ((`elem` s) . snd) raw
    scw3 "PCA All" [ (boxAttr `ofP` mef (NewDimension 3)) raw `preprocess` raw ]
    scw3 ("PCA " ++ concat s) [ (boxAttr `ofP` mef (NewDimension 3)) sel `preprocess` sel ]

    let redu = mef (NewDimension 40) raw `preprocess` raw
        sel = filter ((`elem` s) . snd) redu
        sep = (boxAttr `ofP` mdf) sel `preprocess` sel
    scw3 "MDF " [sep]

    let dig d =  (boxAttr `ofP` mef (NewDimension 3)) they `preprocess` they
          where they = filter ((`elem` [d]) . snd) raw
    scw3 "PCA each" (map (dig.return) "0123456789")

