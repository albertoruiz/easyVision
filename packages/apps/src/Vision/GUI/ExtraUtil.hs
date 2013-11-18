{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
{- |
Module      :  Vision.GUI.Util
Copyright   :  (c) Alberto Ruiz 2012
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

common interfaces

-}
-----------------------------------------------------------------------------

module Vision.GUI.ExtraUtil (
    Vision.GUI.ExtraUtil.camera,
    Vision.GUI.ExtraUtil.run
) where

import Image.Core
import Vision.GUI.Util
import Control.Arrow((***),(>>>),arr)
import Control.Monad((>=>),join)
import Control.Applicative((<*>),(<$>))
import ImagProc(Channels,channelsFromRGB)
import Vision.GUI.Arrow--(ITrans, Trans,transUI,transUI2,runT_)
import Util.LazyIO((~>),(>~>),mkGenerator,Generator)
import Util.Misc(replaceAt,posMin)
import Util.Options
import Control.Concurrent(threadDelay,forkIO)
import Control.Monad(when)

import Text.Printf(printf)
import Data.IORef
import Data.Maybe(fromJust)


camera :: Generator Channels
camera = toRGB Vision.GUI.Util.camera

toRGB :: Generator ImageRGB -> Generator Channels
toRGB = fmap (fmap (fmap channelsFromRGB))

run :: ITrans Channels b -> IO ()
run t = runT_ Vision.GUI.ExtraUtil.camera (t >>> optDo "--freq" freqMonitor)

