-- vim: set et ts=2 sw=2:
{-
Physics of balls in the maze.
-}
module Ball where

import Data.Maybe
import Data.List
import Data.Ord
import Control.Monad
import Numeric.LinearAlgebra
import Vision.Geometry
import Quaternion
import Maze

-- Represent a marble/ball in the maze
data Ball = Ball {
    radius :: Double,
    -- Between 0 and 1, with 1 being perfectly elastic
    elasticity :: Double,
    -- Damping parameter, slows the ball with each timestep
    damping :: Double,
    -- The current orientation of the ball
    orientation :: Quaternion,
    -- The current position of the ball
    position :: Vec3d,
    -- The current velocity of the ball
    v :: Vec3d,
    -- The angular velocity of the ball (currently unused)
    w :: Quaternion
  } deriving (Show)

-- Create a new ball
newBall radius = Ball {
  radius = radius,
  elasticity = 1.0,
  damping = 0.001,
  orientation = qIdent,
  position = vec3 0.5 0.5 radius,
  v = vec3 0 0 0,
  w = qIdent }

-- Constrain a vector to the x-y plane
squash v = mul v $ vec3 1 1 0

norm x = pnorm PNorm2 x

-- Alter the orientation to be consistent with perfect rolling motion in
-- accordance with the current velocity.
roll :: Ball -> Ball
roll ball =
  let Ball { v = v, radius = r, orientation = o } = ball
      direction = squash v
      distance = norm direction
      angle = distance / r
      axis = cross (vec3 0 0 1) direction
      rotation = if norm axis > 0 then qRot angle axis else qIdent
      o' = qProd rotation o
  in ball { orientation = unitary o' }

-- Clamp a vector to the given length
clamp length v =
  let n = norm v
  in if n <= length then v
     else scale (length / n) v

-- Simulate the effect of gravity on the ball (Euler's method)
fall :: Vec3d -> Ball -> Ball
fall g ball =
  let Ball { v = v, position = p, radius = r, damping = f } = ball
      -- do not allow the velocity to exceed the radius, to avoid teleporting
      -- artifacts caused by approximate collision algorithm
      v' = clamp r $ scale (1 - f) $ add v $ squash g
      p' = add p v
  in ball { v = v', position = p' }

-- Recalculate position and velocity of the ball assuming it bounced at time
-- 1-t intervals ago against a surface with normal n.
bounce :: Double -> Vec3d -> Ball -> Ball
bounce t n ball =
  let Ball { v = v, position = p, elasticity = e } = ball
      v' = sub v $ scale ((1+e)*(dot v n)) n
      t' = 1-t
      p' = add (scale t' v') (sub p (scale t' v))
  in ball { v = v', position = p' }

-- Check for collisions with objects in the maze which occurred in the last
-- timestep and bounce the ball if necessary.
maybeBounce :: Maze -> Ball -> Ball
maybeBounce maze ball =
  let obstacles = walls maze ball
      intersections = map (maybeIntersectWall ball) obstacles
      compareIntersections = comparing (fst . fromMaybe (2, vec3 0 0 0))
      intersection = minimumBy compareIntersections (Nothing:intersections)
  in case intersection of
    Nothing -> ball
    Just (t, n) -> maybeBounce maze $ bounce t n ball

-- This seems like a function that should exist in the standard library but I
-- can't find it
listOr [] b = b
listOr a _ = a

-- Given a maze and a ball, return walls which the ball may have bounced
-- against in the last timestep. This does simple visibility culling (near
-- walls block far ones),
walls :: Maze -> Ball -> [Wall]
walls maze Ball { position = p1, v = v } =
  let p0 = sub p1 v
      tile = tileAt maze (p0@>0) (p0@>1)
  in concat [
    tileWall tile MazeUp `listOr`
      (tileWall (tileNeighbor tile MazeUp) MazeLeft `mplus` tileWall (tileNeighbor tile MazeUp) MazeRight),
    tileWall tile MazeDown `listOr`
      (tileWall (tileNeighbor tile MazeDown) MazeLeft `mplus` tileWall (tileNeighbor tile MazeDown) MazeRight),
    tileWall tile MazeRight `listOr`
      (tileWall (tileNeighbor tile MazeRight) MazeDown `mplus` tileWall (tileNeighbor tile MazeRight) MazeUp),
    tileWall tile MazeLeft `listOr`
      (tileWall (tileNeighbor tile MazeLeft) MazeDown `mplus` tileWall (tileNeighbor tile MazeLeft) MazeUp)]

-- If the ball hit this wall, return the time and normal of the hit.
-- Since walls are always orthogonal to an axis we can use a bit less math than
-- if the walls were generally oriented
maybeIntersectWall :: Ball -> Wall -> Maybe (Double, Vec3d)
maybeIntersectWall ball wall@(Wall axis height min max) =
  let Ball { radius = r, position = p1, v = v } = ball
      p0 = sub p1 v
      vx = (v@>axis)
      dir = if vx >= 0 then 1 else (-1)
      r0 = (p0@>axis) + r*dir
      t = (height - r0) / vx
      axis' = otherAxis axis
      yt = (p0@>axis') + t*(v@>axis')
      end y = case axis of
        0 -> (height, y)
        1 -> (y, height)
  in
    if vx /= 0 && t >= 0 && t <= 1 && yt >= min && yt <= max
    -- hit the wall itself
    then Just (t, scale dir $ axisNormal axis)
    -- maybe hit the end of the wall
    else mplus (maybeIntersectPoint ball (end min))
               (maybeIntersectPoint ball (end max))
  where
    axisNormal 0 = vec3 (-1) 0 0
    axisNormal 1 = vec3 0 (-1) 0
    otherAxis 0 = 1
    otherAxis 1 = 0

-- If the ball hit this point, return the time and normal of the hit.
-- We can actually think of this as a ray-circle intersection, since a moving
-- point intersecting a stationary circle is exactly equivalent to a moving
-- circle intersecting a stationary point.
-- Based on the equation: length ((x,y) - t*v - p0) = r
maybeIntersectPoint :: Ball -> (Double, Double) -> Maybe (Double, Vec3d)
maybeIntersectPoint Ball { radius = r, position = p1, v = v } (x,y) =
  let p0 = sub p1 v
      dx = x - (p0@>0)
      dy = y - (p0@>1)
      vx = v@>0
      vy = v@>1
      a = vx*vx + vy*vy
      b = dx*vx + dy*vy
      c = dx*dx + dy*dy - r*r
      t = let s = b*b - a*c
              s' = sqrt s
              t0 = (b - s') / a
          in if s < 0 then -1 else t0
      pt = add p0 (scale t v)
      n = scale (recip r) $ vec3 ((pt@>0) - x) ((pt@>1) - y) 0
  in
    if t >= 0 && t <= 1
    then Just (t, n)
    else Nothing
