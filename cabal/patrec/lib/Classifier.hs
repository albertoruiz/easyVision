-----------------------------------------------------------------------------
{- |
Module      :  Classifier
Copyright   :  (c) Alberto Ruiz 2006-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Algorithms for pattern recognition and machine learning.

-}
-----------------------------------------------------------------------------

module Classifier (
    module Classifier.Base,
    module Classifier.Prepro,
    module Classifier.Simple,
    module Classifier.Kernel,
    module Classifier.Neural,
    module Classifier.Adaboost,
    module Classifier.Tree,
    module Classifier.SVM
) where

import Classifier.Base
import Classifier.Prepro
import Classifier.Simple
import Classifier.Kernel
import Classifier.Neural
import Classifier.Adaboost
import Classifier.SVM
import Classifier.Tree

