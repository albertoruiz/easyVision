-- vim: set et ts=2 sw=2:
{-
Simulation of a marble in a maze. This exists to test the physics and rendering
separately from the camera pose features.

Keys (these are designed for a Dvorak keyboard layout):
h: tilt left
n: tilt right
c: tilt forward
t: tilt back
-}
module Main where

import EasyVision hiding (orientation)
import System.Environment(getArgs)
import qualified Data.Map as Map
import Graphics.UI.GLUT hiding (Matrix, Size, Point, position)
import Vision
import Control.Monad(when)
import Numeric.LinearAlgebra
import Data.Maybe
import Data.IORef
import Control.Concurrent
import Quaternion
import Ball
import Maze

data AppState = AppState

initialState = AppState

-- Sim interval in microseconds, i.e. 500 = 1/20 of a second
simInterval :: Int
simInterval = 500

data Sim = Sim { getBall :: IO Ball, setGravity :: Vec3d -> IO Vec3d, addGravity :: Vec3d -> IO Vec3d }

startSim :: IO Sim
startSim = do
  ballMV <- newMVar $ (newBall 0.3) { elasticity = 0.9 }
  gravityMV <- newMVar $ vec3 0 0 0
  let setBall = swapMVar ballMV
      getBall = readMVar ballMV
      setGravity = swapMVar gravityMV
      getGravity = readMVar gravityMV
      addGravity g' = do
        g <- getGravity
        setGravity $ add g g'
      step = do
        ball <- getBall
        gravity <- getGravity
        setBall $ roll $ maybeBounce maze6x10 $ fall gravity ball
        threadDelay simInterval
        step
  forkIO step
  return $ Sim getBall setGravity addGravity

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

-- Null keyboard callback
nullCallback :: Key -> KeyState -> Modifiers -> Position -> IO ()
nullCallback _ _ _ _ = return ()

tiltCb :: Sim -> KeyboardMouseCallback -> IORef (State AppState) -> KeyboardMouseCallback
tiltCb sim defaultCb _ a@(Char 'h') b@Down c d = do
  addGravity sim $ vec3 (-0.0001) 0 0 
  defaultCb a b c d
tiltCb sim defaultCb _ a@(Char 'h') b@Up c d = do
  addGravity sim $ vec3 0.0001 0 0 
  defaultCb a b c d
tiltCb sim defaultCb _ a@(Char 'n') b@Down c d = do
  addGravity sim $ vec3 0.0001 0 0
  defaultCb a b c d
tiltCb sim defaultCb _ a@(Char 'n') b@Up c d = do
  addGravity sim $ vec3 (-0.0001) 0 0
  defaultCb a b c d
tiltCb sim defaultCb _ a@(Char 'c') b@Down c d = do
  addGravity sim $ vec3 0 0.0001 0
  defaultCb a b c d
tiltCb sim defaultCb _ a@(Char 'c') b@Up c d = do
  addGravity sim $ vec3 0 (-0.0001) 0
  defaultCb a b c d
tiltCb sim defaultCb _ a@(Char 't') b@Down c d = do
  addGravity sim $ vec3 0 (-0.0001) 0
  defaultCb a b c d
tiltCb sim defaultCb _ a@(Char 't') b@Up c d = do
  addGravity sim $ vec3 0 0.0001 0
  defaultCb a b c d
tiltCb _ defaultCb _ a b c d = defaultCb a b c d

main = do
  size <- findSize
  (trackball,kc,mc) <- newTrackball
  app <- prepare' initialState
  sim <- startSim
  addWindow "sim" size Nothing (tiltCb sim $ kc nullCallback) app
  motionCallback $= Just mc
  depthFunc $= Just Less
  launch' app $ worker trackball sim

worker trackball sim inWindow state = do
  inWindow "sim" $ do
    clear [ColorBuffer, DepthBuffer]
    trackball
    -- look at the center of the maze
    translate $ Vector3 ((paperMinX-paperMaxX)/2) ((paperMinY-paperMaxY)/2) 0
    -- draw the maze as dark red walls with bright red edges
    setColor 0.5 0 0
    renderPrimitive Quads (mapM_ vertex $ mazeQuads maze6x10 0 0.5)
    setColor 1 0 0
    lineWidth $= 3
    renderPrimitive Lines (mapM_ vertex $ mazeQuadLines maze6x10 0 0.5)
    -- draw the ball
    ball <- getBall sim
    renderBall ball
  return state

paperMinX = 0 :: Double
paperMaxX = 6 :: Double
paperMinY = 0 :: Double
paperMaxY = 10 :: Double
paper =
  [[paperMinX, paperMinY],
   [paperMinX, paperMaxY],
   [paperMaxX, paperMaxY],
   [paperMaxX, paperMinY]]
