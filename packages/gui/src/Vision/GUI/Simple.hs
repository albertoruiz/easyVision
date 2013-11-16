-----------------------------------------------------------------------------
{- |
Module      :  Vision.GUI
Copyright   :  (c) Alberto Ruiz 2007-11
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Vision.GUI.Simple (
    module Vision.GUI.Util,
    module Vision.GUI.Arrow,
    module Vision.GUI.Draw,
    module Vision.GUI.Interface,
    module Vision.GUI.Parameters,
    module Vision.GUI.Objects,
    module Vision.GUI.Types,
    module Data.Colour.Names,
    module Control.Arrow,
    module Control.Applicative,
    module Data.Function,
    module Text.Printf,
    module Data.List
)where

import Vision.GUI.Util
import Vision.GUI.Arrow
import Vision.GUI.Types
import Vision.GUI.Interface
import Vision.GUI.Parameters
import Vision.GUI.Objects
import Vision.GUI.Draw
import Data.Colour.Names(black,white,gray,red,green,blue,yellow,pink,orange,violet,
                         darkgray,lightgray,lightgreen,lightblue)
import Control.Arrow
import Control.Applicative((<$>))
import Data.Function(on)
import Text.Printf
import Data.List(sort,sortBy,maximumBy,minimumBy)

