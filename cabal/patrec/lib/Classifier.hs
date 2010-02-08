{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  Classifier
Copyright   :  (c) Alberto Ruiz 2006-7
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Algorithms for pattern recognition and machine learning.

-}
-----------------------------------------------------------------------------

module Classifier (
    module Classifier.Base,
    module Classifier.Simple,
    module Classifier.Kernel,
    module Classifier.Neural,
    module Classifier.Adaboost,
    module Classifier.SVM
) where

import Classifier.Base
import Classifier.Simple
import Classifier.Kernel
import Classifier.Neural
import Classifier.Adaboost
import Classifier.SVM
