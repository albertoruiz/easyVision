-----------------------------------------------------------------------------
{- |
Module      :  Classifier.SVM
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  uses SVM-Light

Interface to the Support Vector Machine program SVM-Light by Thorsten Joachims, available in <http://svmlight.joachims.org/>. The binary @svm_learn@ must be in the PATH.

-}
-----------------------------------------------------------------------------

module Classifier.SVM (
    svmLight
) where

import Numeric.LinearAlgebra.HMatrix
import Classifier.Base
import Classifier.Kernel(gaussK)
import System.Process (system)
import System.IO.Unsafe (unsafePerformIO)
import Util.Misc(vec,Vec)

-- | Creates a support vector machine with gaussian kernel of given width.
svmLight :: Double -> Dicotomizer
svmLight w gs = unsafePerformIO $ do
    -- FIXME: use unique filenames
    _ <- svmLearn w gs "prob-svm.txt" "model.txt"
    (b,ws,yvecs) <- svmReadModel "model.txt"
    let n = fromIntegral (length yvecs)
        f v = (b + sum (map q yvecs)) / n where q (y,x) = y * gaussK ws x v
    _ <- system "rm model.txt"
    _ <- system "rm prob-svm.txt"
    return f

svmLearn :: (Show a) => a -> ([Vec], [Vec]) -> FilePath -> String -> IO ()
svmLearn w (pos,negs) fileprob filemodel = do
    writeFile fileprob cad
    _ <- system $ "svm_learn -t 2 -g "++ show w ++" "++fileprob++" "++ filemodel++" > /dev/null"
    -- system $ "svm_learn -t 2 -x 1 -g "++ show w ++" "++fileprob++" "++ filemodel++" >> svm.log"
    return ()
  where cad = unlines $ header ++ map (feat "+1") pos ++ map (feat "-1") negs
        header = ["# automatically generated file","#"]
        feat c ex = c ++ ve ex
        ve v = concat $ zipWith shAtt [1::Int ..] (toList v)
        shAtt i x  = ' ':(show i)++':':(show x)

svmReadModel :: FilePath -> IO (Double, Double, [(Double, Vec)])
svmReadModel file = do
    ss <- lines `fmap` readFile file
    let spars = take 11 ss
        svecs = drop 11 ss
        param k = read . head . words $ spars !! k
        n     = param 7 :: Int
        b     = param 10 :: Double
        w     = param 3  :: Double
        yvecs  = map feat svecs
        feat line = (read sy :: Double , vec $ map att sxs)
            where (sy:sxs) = take (n+1) $ words line
                  att = read . tail . snd . span (/=':')
    return (b,w,yvecs)
