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
    Vision.GUI.ExtraUtil.camera
) where

--import Graphics.UI.GLUT hiding (Point,Size,color)
import Vision.GUI
import Control.Arrow((***),(>>>),arr)
import Control.Monad((>=>),join)
import Control.Applicative((<*>),(<$>))
import ImagProc
import ImagProc.Camera(findSize,readFolderMP,readFolderIM,getCam)
import Vision.GUI.Arrow--(ITrans, Trans,transUI,transUI2,runT_)
import Util.LazyIO((~>),(>~>),mkGenerator,Generator)
import Util.Misc(replaceAt,posMin)
import Util.Options
import Control.Concurrent(threadDelay,forkIO)
import Control.Monad(when)

import Text.Printf(printf)
import Data.IORef
import Data.Maybe(fromJust)


{- |
Returns the first image source given in the command line.

By default it uses the first alias in cameras.def.

It also admits --photos=path/to/folder/ containing separate image files (.jpg or .png), currently read using imagemagick (slower, lazy),
and --photosmp=path/to/folder, to read images of the same type and size using mplayer (faster, strict).
-}
camera :: Generator Channels
camera = do
    f <- hasValue "--photos"
    g <- hasValue "--photosmp"
    h <- hasValue "--sphotos"
    if f then toRGB cameraFolderIM
         else if g then toRGB cameraFolderMP
                   else if h then cameraP
                             else cameraV

toRGB = fmap (fmap (fmap channelsFromRGB))


cameraP :: Generator Channels
cameraP = do
    hp <- optionString "--sphotos" "."
    g <- readFolderIM hp
    c <- mkGenerator g
    return (fmap fst <$> c)

cameraV :: Generator Channels
cameraV = findSize >>= getCam 0 ~> channels


cameraFolderMP :: Generator ImageRGB
cameraFolderMP = camG "--photosmp" (rfmp)  <*> dummy
  where
    rfmp op sz = fmap (fmap (rgb *** id)) (readFolderMP op sz)

dummy :: Generator ()
dummy = return (threadDelay 100000 >> return (Just ()))

