-- vim: set et ts=2 sw=2:
{-
Representation of walls in the maze.
-}
module Maze where

import Data.List
import Data.Array
import Control.Monad

maze3x5 = maze $ ""
  ++ "|¯|¯¯¯|\n"
  ++ "| | | |\n"
  ++ "|   | |\n"
  ++ "| | | |\n"
  ++ "| |  ¯|\n"
  ++ "¯¯¯¯¯¯¯"

maze6x10 = maze $ ""
  ++ "|¯|¯¯¯|¯|¯¯¯|\n"
  ++ "| | | | | | |\n"
  ++ "|   |     | |\n"
  ++ "|  ¯|  ¯| | |\n"
  ++ "| |  ¯| |  ¯|\n"
  ++ "|¯|¯¯¯¯¯|¯¯ |\n"
  ++ "|   | | | | |\n"
  ++ "|¯¯ |  ¯¯ | |\n"
  ++ "| | | | | | |\n"
  ++ "| |  ¯| |  ¯|\n"
  ++ "¯¯¯¯¯¯¯¯¯¯¯¯¯"

data MazeDirection = MazeUp | MazeDown | MazeLeft | MazeRight

-- Each cell in the maze may have a top wall and/or a right wall. The bottom
-- and left walls of a cell may be provided by the next cell down and right
-- respectively.
data Walls = Walls { topWall :: Bool, rightWall :: Bool }

-- ASCII art for walls
instance Show Walls where
  show (Walls True True)  = "¯|"
  show (Walls True False) = "¯¯"
  show (Walls False True) = " |"
  show (Walls False False) = "  "

-- Note that tiles are addressed in row-major order. Which means that the
-- layout of tiles in memory does not match the intuitive addressing of tiles
-- in space. The transformation is handled by the function which loads the data
-- into the maze, so that (x,y) addresses work as expected.
data Maze = Maze (Array (Int, Int) Walls)

-- Display a maze as ASCII art
instance Show Maze where
  show maze@(Maze walls) =
    let ((x0,y0), (x1,y1)) = bounds walls
        row y = concatMap show rowWalls
          where rowWalls = map (\x -> wallsAt maze x y) [x0-1 .. x1]
    in unlines $ map row $ reverse [y0-1 .. y1]

-- A location in a maze
data Tile = Tile Maze (Int, Int) deriving (Show)

-- Generate a maze from an ASCII-art description
maze string =
  let rows = map parseLine $ map tail $ init $ lines string
      h = length rows
      w = maximum $ map length rows
      parseLine (a:b:rest) = (parseWall [a,b]):(parseLine rest)
      parseLine (a:[]) = error "maze: wrong number of characters in line"
      parseLine [] = []
      parseWall "¯|" = Walls True True
      parseWall "¯¯" = Walls True False
      parseWall " |" = Walls False True
      parseWall "  " = Walls False False
      parseWall other = error $ "maze: unrecognized tile " ++ other
  in Maze $ listArray ((0,0), (w-1, h-1)) $ concat $ transpose $ reverse rows

-- Get the walls (top/right) at a location in the maze. This creates wall
-- information on-the-fly for out-of-bounds locations, most usefully for the
-- left and bottom sides of the maze.
wallsAt (Maze walls) x y = 
  let ((x0,y0), (x1,y1)) = bounds walls
  in if (x > x1 || y > y1)
     then Walls False False
     else if (x < x0 || y < y0)
     then Walls (y < y0 && x >= x0) (x < x0 && y >= y0)
     else walls ! (x, y)

-- Return a list of endpoints of walls in the maze, suitable for rendering in
-- OpenGL as Lines.
mazeVertices :: Maze -> [[Double]]
mazeVertices maze@(Maze walls) =
  do
    let ((x0,y0), (x1,y1)) = bounds walls
    x <- [x0-1..x1]
    y <- [y0-1..y1]
    let xmin = fromIntegral x
        xmax = xmin + 1
        ymin = fromIntegral y
        ymax = ymin + 1
        Walls top right = wallsAt maze x y
    mplus
      (if top then [[xmin,ymax], [xmax,ymax]] else [])
      (if right then [[xmax,ymin], [xmax,ymax]] else [])

-- Return a list of vertices of walls in the maze, suitable for rendering in
-- OpenGL as Quads.
mazeQuads maze z0 z1 = extrude $ mazeVertices maze
  where extrude (p1:p2:rest) = [p1++[z0], p2++[z0], p2++[z1], p1++[z1]] ++ (extrude rest)
        extrude [] = []

-- Return a list of vertices of walls in the maze, suitable for rendering in
-- OpenGL as Lines.
mazeQuadLines maze z0 z1 = quadsToLines $ mazeQuads maze z0 z1
  where quadsToLines (a:b:c:d:rest) = a:b:b:c:c:d:d:a:(quadsToLines rest)
        quadsToLines [] = []

-- Return the size of the maze in x/y units
mazeSize :: Maze -> (Double, Double)
mazeSize (Maze walls) = (fromIntegral (x1+1-x0), fromIntegral (y1+1-y0))
  where ((x0,y0), (x1,y1)) = bounds walls

-- Return the tile at a given x/y coordinate in the maze.
tileAt maze x y = Tile maze (floor x, floor y)

-- Return the walls (top/right) at a location in the maze
tileWalls (Tile maze (x, y)) = wallsAt maze x y

-- Return the boundary value of a tile in the given direction. E.g. MazeUp
-- returns the upper y bound. Bounds are [min, max).
tileBound (Tile _ (x,y)) dir = case dir of
  MazeUp    -> fromIntegral $ y+1
  MazeDown  -> fromIntegral y
  MazeLeft  -> fromIntegral x
  MazeRight -> fromIntegral $ x+1

-- Return the neighboring tile in the given direction. E.g. MazeUp returns the
-- nearest tile in the +y direction.
tileNeighbor (Tile m (x, y)) dir = Tile m $ case dir of
  MazeUp    -> (x, y+1)
  MazeDown  -> (x, y-1)
  MazeLeft  -> (x-1, y)
  MazeRight -> (x+1, y)

-- A wall in the maze. Note that walls are always axis-aligned so they are
-- simpler to work with than general lines.
data Wall = Wall {
  -- Which axis is this wall perpendicular to (0 for x, 1 for y)
  wallAxis :: Int,
  -- Where on the axis does this wall lie
  wallHeight :: Double,
  -- What is the lower boundary of this wall?
  wallMin :: Double,
  -- What is the upper boundary of this wall?
  wallMax :: Double } deriving (Show)

-- Return the wall in the given direction from the given tile if one exists.
tileWall tile dir = case dir of
  MazeUp    -> if topWall   $ tileWalls $ tile
    then return $ Wall 1 (tileBound tile dir) (tileBound tile MazeLeft) (tileBound tile MazeRight)
    else mzero
  MazeDown  -> if topWall   $ tileWalls $ tileNeighbor tile dir
    then return $ Wall 1 (tileBound tile dir) (tileBound tile MazeLeft) (tileBound tile MazeRight)
    else mzero
  MazeLeft  -> if rightWall $ tileWalls $ tileNeighbor tile dir
    then return $ Wall 0 (tileBound tile dir) (tileBound tile MazeDown) (tileBound tile MazeUp)
    else mzero
  MazeRight -> if rightWall $ tileWalls $ tile
    then return $ Wall 0 (tileBound tile dir) (tileBound tile MazeDown) (tileBound tile MazeUp)
    else mzero
