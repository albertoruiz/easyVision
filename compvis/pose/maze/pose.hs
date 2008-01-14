-- vim: set et ts=2 sw=2:
-- ./pose tv:// --focal 2.45
--
-- The majority of this file is  Copyright   :  (c) Alberto Ruiz 2006-7
-- Minor modifications by Adrian Quark

module Main where

import EasyVision
import System.Environment(getArgs)
import qualified Data.Map as Map
import Graphics.UI.GLUT hiding (Matrix, Size, Point)
import Vision
import Control.Monad
import Control.Exception
import Numeric.LinearAlgebra
import Burns
import ExtractQuads

main = do
  sz <- findSize
  focal <- maybeOption "--focal" :: IO (Maybe Double)
  (cam, ctrl) <- getCam 0 (Size 120 160) >>= withPause

  prepare
  params <- createParameters [
              ("min line length",realParam 10 0 30),
              ("max line distance",realParam 0.06 0 0.10),
              ("min gradient",intParam 20 0 100),
              ("buckets",intParam 8 4 10),
              ("scale",realParam 0.2 0.01 1),
              ("ortho tolerance",realParam 0.5 0.01 1.0)]
  windowImage <- evWindow () "image" sz Nothing (const $ kbdcam ctrl)
  window3D <- evWindow3D () "3D view" 400 (const $ kbdcam ctrl)
  -- initialize some opengl stuff
  -- (now automatic)

  launch (worker windowImage window3D cam params focal)

-----------------------------------------------------------------


worker windowImage window3D cam params focal = do

  minlen <- getParam params "min line length"
  maxdis <- getParam params "max line distance"
  mingrad <- getParam params "min gradient"
  buckets <- getParam params "buckets"
  scale  <- getParam params "scale"
  orthotol  <- getParam params "ortho tolerance"

  --orig <- read_bmp "burns/demo.bmp" >>= rgbToGray
  orig <- cam >>= yuvToGray
  let segs = burns_line_extraction orig buckets mingrad minlen
      closed4 = [p | Closed p <- extractQuads maxdis segs]

  inWin windowImage $ do
    drawImage orig

    pointCoordinates (size orig)

    setColor 0 0 1
    lineWidth $= 2
    renderPrimitive Lines $ mapM_ drawSeg segs

    setColor 1 0 0
    lineWidth $= 3
    mapM_ (renderPrimitive LineLoop . (mapM_ vertex)) closed4

    setColor 0 1 0
    pointSize $= 5
    mapM_ (renderPrimitive Points . (mapM_ vertex)) closed4

  papers <- filterM (isA4 focal orthotol) (concatMap alter closed4)

  inWin window3D $ do

    setColor 0 0 1
    lineWidth $= 2
    renderPrimitive LineLoop (mapM_ vertex paper)

    when (length papers >0) $ do
      let pts = head papers
          h = estimateHomography paper (map pl pts)
          Size _ sz = size orig
      imf <- scale8u32f 0 1 orig
      let floor = warp 0 (Size 256 256) (scaling scale <> h) imf
      drawTexture floor $ map (++[-0.01]) $ ht (scaling (1/scale)) [[1,1],[-1,1],[-1,-1],[1,-1]]

      imt <- extractSquare 128 imf
      let Just (cam,path) = cameraFromPlane 1E-3 500 focal (map pl pts) paper
      --let Just cam = cameraFromHomogZ0 focal (inv h)
      drawCamera 1 cam (Just imt)

      pointCoordinates (Size 400 400)
      setColor 1 1 1
      text2D 0.95 (-0.95) (show $ focalFromHomogZ0 $ inv h)

  return ()

---------------------------------------------------------

paper = [[0,   0],
         [0,   2.5],
         [1.5, 2.5],
         [1.5, 0]]

-- The following functions were copied from Alberto Ruiz's dynamic.hs

pl (Point x y) = [x,y]

alter pts = map (rotateList pts) [0 .. 3]

rotateList list n = take (length list) $ drop n $ cycle list

drawSeg s = do
  vertex $ (extreme1 s)
  vertex $ (extreme2 s)

isA4 focal tol pts =
  -- Somewhere in here an HSSL function can be called which responds by
  -- throwing an error. Bad library design!
  catchJust errorCalls (evaluate $ ao < tol && cy < 0) (const $ return False)
  where
    mbomega = fmap omegaGen focal
    ao = autoOrthogonality mbomega h
    h = estimateHomography (map pl pts) paper
    Just p = poseFromHomogZ0 focal h
    (_,cy,_) = cameraCenter p

omegaGen f = kgen (recip (f*f))
