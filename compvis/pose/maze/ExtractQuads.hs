-- vim: set et ts=2 sw=2:
{-
Recognize polygons from unconnected line segments.
Yes, this duplicates
-}
module ExtractQuads ( extractQuads ) where

import Data.List
import Data.Array
import Control.Monad
import Data.Graph
import Vision.Geometry
import Numeric.LinearAlgebra
import ImagProc.Ipp
import ImagProc
--import Polyline
--import Queue

newQ l = l
emptyQ l = l == []
deQ (x:xs) = (x,xs)
enQ xs x = xs++[x] 

type Segments = Array Int Segment

instance Show Segment where
  show (Segment p1 p2) = show (p1,p2)

-- There exists an edge between two segments if the minimum distance between
-- endpoints of the segments is less than the threshold.
segmentsGraph :: Double -> Segments -> Graph
segmentsGraph thresh segments =
  let 
      adjacent p1 p2 = if distPoints p1 p2 <= thresh then True else False
      edge (i1, Segment p11 p12) (i2, Segment p21 p22) = do
        p1 <- [p11, p12]
        p2 <- [p21, p22]
        guard $ adjacent p1 p2
        return (i1, i2)
      edges (a:rest) = (concat $ map (edge a) rest) ++ (edges rest)
      edges [] = []
      undirected ((x,y):rest) = (x,y) : (y,x) : undirected rest
      undirected [] = []
      es = undirected $ edges (assocs segments)
      bs = bounds segments
  in buildG bs es

{-
Convert a list of segments to a polyline by finding corners.
Segments are converted to lines and intersected using homogeneous coordinates
as described in http://en.wikipedia.org/wiki/Incidence_(geometry).
This code was adapted from Alberto Ruiz's ImageProc.Segments.improvePoints
-}
segmentsToPolygon :: [Segment] -> Polyline
segmentsToPolygon ss =
  let lines@(firstLine:_) = map s2l ss
  in clockwise $ Closed $ corners' (lines ++ [firstLine])
  where 
    s2l (Segment p1 p2) = cross (p2v p1) (p2v p2)
    p2v (Point x y) = 3 |> [x,y,1]
    v2p v = let [x,y] = toList $ inHomog v in Point x y
    corners' (_:[]) = []
    corners' (a:rest@(b:_)) = v2p (cross a b) : corners' rest
    clockwise poly@(Closed points) =
      if orientation poly < 0 then poly else Closed (reverse points)

{-
Return a list of cycles (lists of vertices) in order from shortest to longest.
This is not the most efficient algorithm but it's particularly well-suited to
this application because we only need to find closed paths of a certain size
and lazy lists let us do that easily and efficiently.

Based on the algorithm described in:

  @Article{ Liu1,
    title = "A new way to enumerate cycles in graph",
    author = "Hongbo Liu and Jiaxin Wang",
    journal = "Telecommunications, 2006. AICT-ICIW '06.",
    pages = "57--57",
    volume = "19",
    number = "25",
    month = feb
    year = "2006",
  }
-}
cycles :: Graph -> [[Vertex]]
cycles graph = cycles' (newQ $ map (:[]) (vertices graph)) where
  -- qs is a queue of open paths to investigate
  cycles' q
    | emptyQ q = []
    | otherwise =
      let (p, q') = deQ q
          -- Paths are stored as backwards lists to make appending efficient
          vh = last p
          vt = head p
          vxs = graph ! vt
          -- Enqueue all single-vertex extensions of the current path and
          -- find more cycles
          cs = cycles' $ foldr (flip enQ) q' [vx:p | vx <- vxs \\ p, vx > vh]
      in if vh `elem` vxs then (reverse p):cs else cs

labelSegments :: [Segment] -> Segments
labelSegments segments = listArray (1, length segments) segments

quadIsSane :: Polyline -> Bool
quadIsSane (Closed points) = all pointIsSane points
  where
    pointIsSane (Point x1 y1) = x1 >= (-1) && x1 <= 1 && y1 >= (-1) && y1 <= 1

extractQuads :: Double -> [Segment] -> [Polyline]
extractQuads thresh segmentList =
  let segments = labelSegments segmentList
      graph = segmentsGraph thresh segments
      cycles4 = takeWhile ((==4) . length) $ dropWhile ((<4) . length) $ cycles graph
  in filter quadIsSane $ map (segmentsToPolygon . (map (segments!))) cycles4
