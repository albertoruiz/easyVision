-- vim: set et ts=2 sw=2:
{-
This is an augmented-reality interface to a simple physical simulation of a
marble maze. The program tracks a 3x5 notecard against a dark background using
a low-resolution webcam. It displays the image viewed by the webcam with a
virtual marble maze superimposed over the notecard. As the notecard moves, the
maze moves correspondingly in real-time, with the marble responding to this
motion in a physically-plausible way. For example, when the maze is tilted, the
marble rolls. The goal of this project is a basic understanding of camera pose
estimation from known image geometry.

Keys:
a: set neutral position
m: toggle mirroring of image
-}
module Main where

import EasyVision hiding (orientation)
import System.Environment(getArgs)
import qualified Data.Map as Map
import Graphics.UI.GLUT hiding (Matrix, Size, Point, position)
import qualified Graphics.UI.GLUT as GLUT
import Text.Printf
import Debug.Trace
import Vision
import Control.Monad(when)
import Numeric.LinearAlgebra
import Data.Maybe
import Data.IORef
import Control.Concurrent
import Control.Monad
import Control.OldException
import ExtractQuads
import Quaternion
import ImagProc.C.Burns
import Ball
import Maze

-- Application-specific state
data AppState = AppState {
  -- Neutral orientation of the camera
  levelCamera :: Matrix Double,
  -- Last good camera orientation
  lastCamera  :: Matrix Double,
  -- Whether to reset the levelCamera when a good orientation is next acquired
  resetCamera :: Bool,
  -- Whether to mirror the image left-right
  mirrorImage :: Bool }

initialState = AppState {
  levelCamera = cameraAtOrigin,
  lastCamera  = cameraAtOrigin,
  resetCamera = True,
  mirrorImage = True }

-- Simulation interval in microseconds, i.e. 500 = 20 Hz
simInterval :: Int
simInterval = 500

-- "Object" with methods for interacting with an ongoing simulation
data Sim = Sim { getBall :: IO Ball, setGravity :: (Vector Double) -> IO (Vector Double) }

-- Shorthand
vertex3d a b c = vertex $ Vertex3 a b (c::GLdouble)

-- Start a new simulation of a ball in the maze. This implementation using
-- forkIO was adapted from Alberto Ruiz's dynamic.hs example.
startSim :: IO Sim
startSim = do
  ballMV <- newMVar $ (newBall 0.3) { elasticity = 0.1 }
  gravityMV <- newMVar $ vec3 0 0 0
  let setBall = swapMVar ballMV
      getBall = readMVar ballMV
      setGravity = swapMVar gravityMV
      getGravity = readMVar gravityMV
      step = do
        ball <- getBall
        gravity <- getGravity
        -- actually performs the simulation
        setBall $ roll $ maybeBounce maze6x10 $ fall gravity ball
        threadDelay simInterval
        step
  forkIO step
  return $ Sim getBall setGravity

-- Render a ball
renderBall ball = do
  -- locate and orient the ball
  let Ball { radius = r, position = p, orientation = o } = ball
      [x, y, z] = toList p
  translate $ Vector3 x y z
  rotation <- newMatrix RowMajor $ qMatrix o :: IO (GLmatrix GLdouble)
  multMatrix rotation
  -- render the ball as a solid with wireframe edges
  setColor 0 0 1
  renderObject Solid (Sphere' (r*0.98) 10 6)
  lineWidth $= 1
  setColor 1 1 1
  renderObject Wireframe (Sphere' (r*0.99) 10 6)

-- Create a new (chainable) handler for a single key-down event
newCallback :: Key -> (AppState -> IO AppState)
  -> (IORef (State AppState) -> KeyboardMouseCallback)
  -> (IORef (State AppState) -> KeyboardMouseCallback)
newCallback key action defaultCb app a b c d =
  if key == a && b == Down
  then do
    s <- readIORef app
    ust' <- action (ust s)
    writeIORef app (s { ust = ust' })
  else defaultCb app a b c d

-- Keyboard handlers
resetCb = newCallback (Char 'a') $ \s -> return s { resetCamera = True }
mirrorCb = newCallback (Char 'm') $ \s -> return s { mirrorImage = not (mirrorImage s) }

-- Null keyboard callback
nullCallback :: KeyboardMouseCallback
nullCallback _ _ _ _ = return ()

-- Main function
main = do
  size <- findSize
  focal <- maybeOption "--focal"
  (cam, camctl) <- getCam 0 size >>= withPause

  app <- prepare' initialState
  params <- createParameters [
              ("min line length",realParam 10 0 20),
              ("max line distance",realParam 0.02 0 0.1),
              ("min gradient",intParam 25 0 100),
              ("buckets",intParam 7 4 10),
              ("ortho tolerance",realParam 0.5 0.01 1.0)]

  addWindow "sim" size Nothing (mirrorCb $ resetCb $ const $ kbdcam camctl) app
  depthFunc $= Just Less
  sim <- startSim
  launch' app $ worker cam params focal sim

-- Handle refresh of the main window
worker cam params focal sim inWindow state = do
  minlen <- getParam params "min line length" ::IO Double
  maxdis <- getParam params "max line distance"
  mingrad <- getParam params "min gradient" :: IO Int
  buckets <- getParam params "buckets"::IO Int
  orthotol  <- getParam params "ortho tolerance"

  -- get a grayscale image from the webcam

  let pm = if mirrorImage state then mirror8u 1 else id

  image <- cam >>= return . pm . yuvToGray

  -- Extract segments using Burns' line extraction algorithm and find 4-sided
  -- polygons using extractQuads.
  let segs = burns_line_extraction image buckets mingrad minlen
             --segments 4 1.5 5 40 20 True image

  let  closed4 = [p | Closed p <- extractQuads maxdis segs]

  -- Estimate a new camera pose from the first viable-looking rectangle. If
  -- none is found, reuse the last camera pose.
  corners <- filterM (isA4 focal orthotol) (concatMap alter closed4)
  let tryCam corners = fmap fst $ cameraFromPlane 1E-3 500 focal (map pl corners) paper
      maybeCamera = tryCam =<< maybeHead corners
      camera = fromMaybe (lastCamera state) maybeCamera
      state' = if resetCamera state && isJust maybeCamera
        then state { resetCamera = False, levelCamera = camera, lastCamera = camera }
        else state { lastCamera = camera }

  inWindow "sim" $ do
    clear [DepthBuffer]
    drawImage image
    clear [DepthBuffer]
    pointCoordinates (size image)

    -- Draw extracted line segments
    setColor 0 0 1
    lineWidth $= 1
    renderPrimitive Lines $ mapM_ drawSeg segs

    when (not $ resetCamera state') $ do
      clear [DepthBuffer]
      cameraView camera (4/3) 0.1 100

      -- Draw the maze
      setColor 0.5 0 0
      renderPrimitive Quads (mapM_ vertex $ mazeQuads maze6x10 0 0.5)
      setColor 1 0 0
      lineWidth $= 3
      renderPrimitive Lines (mapM_ vertex $ mazeQuadLines maze6x10 0 0.5)

      -- Draw the ball
      ball <- getBall sim
      renderBall ball

      -- Calculate gravity
      let CamPar { tiltAngle = levelTilt, rollAngle = levelRoll } = poseFromCamera (levelCamera state')
          CamPar { tiltAngle = newTilt, rollAngle = newRoll } = poseFromCamera camera
          tilt = newTilt - levelTilt
          roll = newRoll - levelRoll
          gx = sin roll
          gy = sin tilt
      setGravity sim $ vec3 (gx/8000) (gy/8000) 0

      -- Display roll and tilt
      pointCoordinates (Size 400 400)
      setColor 1 1 1
      text2D 0.95 (-0.95) $ printf "(%+3.0f, %+3.0f)" (rad2deg roll) (rad2deg tilt)
      return ()

  return state'

rad2deg r = 360*r/(2*pi)

-- Dimensions of paper
paperMinX = 0
paperMaxX = 6
paperMinY = 0
paperMaxY = 10
paper =
  [[paperMinX, paperMinY],
   [paperMinX, paperMaxY],
   [paperMaxX, paperMaxY],
   [paperMaxX, paperMinY]]

-- The following functions were copied from Alberto Ruiz's dynamic.hs

pl (Point x y) = [x,y]

alter pts = map (rotateList pts) [0 .. 3]
  where rotateList list n = take (length list) $ drop n $ cycle list

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

maybeHead [] = Nothing
maybeHead (a:_) = Just a
