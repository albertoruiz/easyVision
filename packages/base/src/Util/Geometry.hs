{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
{- |
Module      :  Util.Geometry
Copyright   :  (c) Alberto Ruiz 2006-12
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Projective geometry utilities.

-}
-----------------------------------------------------------------------------

module Util.Geometry
(
  -- * Basic Types
    Point(..), HPoint(..), HLine(..),
    Point3D(..), HPoint3D(..), HLine3D(..), HPlane(..),

    Homography, Camera, Homography3D,

    Conic, DualConic, Quadric, DualQuadric,

    Vectorlike(..), Matrixlike(..), Tensorial(..),

    Vect2D(..),

  -- * Transformations

    Transformable(..), Composable(..), Invertible(..), BackTransformable(..),

  -- * Geometric constructions
    Meet(..), Join(..),

  -- * Conversion
    Inhomog(..), Homog(..),

  -- * Derived types

    Polyline(..), Segment(..),
    interPoint, normalSegment,
    segmentLength, distPoints, bounding, cosAngleSegments,
    asSegments, isLeft,
    segmentIntersection, intersectionLineSegment,
    Polygon(..),
    orientation, polygonSides,
    DMat
) where

import Util.Small
import Util.Misc(Mat,Vec)
import Numeric.LinearAlgebra hiding ((|>))
import Numeric.HMatrix(udot)
import Data.Function(on)
import Foreign.Storable
import Foreign.Ptr
import Control.Applicative
import qualified Numeric.LinearAlgebra.Tensor as T
import qualified Numeric.LinearAlgebra.Array.Util as UT
import qualified Numeric.LinearAlgebra.Exterior as E

----------------------------------------------------------------------

-- | inhomogenous 2D point
data Point = Point !Double !Double
  deriving (Eq, Show, Read)

instance Storable Point where
  sizeOf _ = 2*sizeOf (undefined::Double)
  alignment _ = alignment (undefined::Double)
  peek p = do
    let pb = castPtr p
    Point <$> peekElemOff pb 0 <*> peekElemOff pb 1

  poke p (Point x y) = do
    let pb = castPtr p
    pokeElemOff pb 0 x
    pokeElemOff pb 1 y


instance Shaped Point where
    type Shape Point = Dim2 Double
    toArray (Point x1 x2) = vec2 x1 x2
    unsafeFromArray v = Point (v@>0) (v@>1)


-- | 2D displacement
data Vect2D = Vect2D !Double !Double
  deriving (Eq, Show, Read)


-- | inhomogenous 2D point
data HPoint = HPoint !Double !Double !Double deriving (Eq, Show, Read)

instance Shaped HPoint where
    type Shape HPoint = Dim3 Double
    toArray (HPoint x y w) = vec3 x y w
    unsafeFromArray v = HPoint (v@>0) (v@>1) (v@>2)


-- | inhomogenous 3D point
data Point3D = Point3D !Double !Double !Double deriving (Eq, Show, Read)

instance Shaped Point3D where
    type Shape Point3D = Dim3 Double
    toArray (Point3D x y w) = vec3 x y w
    unsafeFromArray v = Point3D (v@>0) (v@>1) (v@>2)


-- | homogenous 3D point
data HPoint3D = HPoint3D !Double !Double !Double !Double deriving (Eq, Show, Read)

instance Shaped HPoint3D where
    type Shape HPoint3D = Dim4 Double
    toArray (HPoint3D x y z w) = vec4 x y z w
    unsafeFromArray v = HPoint3D (v@>0) (v@>1) (v@>2) (v@>3)



-- | 2D line
data HLine = HLine !Double !Double !Double deriving (Eq, Show, Read)

instance Shaped HLine where
    type Shape HLine = Dim3 Double
    toArray (HLine a b c) = vec3 a b c
    unsafeFromArray v = HLine(v@>0) (v@>1) (v@>2)


-- | 3D line (provisional)
newtype HLine3D = HLine3D Mat deriving (Eq, Show, Read)

instance Shaped HLine3D where
    type Shape HLine3D = Dim4x4
    toArray (HLine3D m) = m
    unsafeFromArray = HLine3D


-- | 3D plane
data HPlane = HPlane !Double !Double !Double !Double deriving (Eq, Show, Read)

instance Shaped HPlane where
    type Shape HPlane = Dim4 Double
    toArray (HPlane a b c d) = vec4 a b c d
    unsafeFromArray v = HPlane (v@>0) (v@>1) (v@>2) (v@>3)


--------------------------------------------------------------------------------

-- | projective transformation P2->P2
newtype Homography = Homography Mat deriving (Eq, Show, Read)

instance Shaped Homography where
    type Shape Homography = Dim3x3
    toArray (Homography m) = m
    unsafeFromArray = Homography

-- | projective transformation P3->P2
newtype Camera = Camera Mat deriving (Eq, Show, Read)

instance Shaped Camera where
    type Shape Camera = Dim3x4
    toArray (Camera m) = m
    unsafeFromArray = Camera

-- | projective transformation P3->P3
newtype Homography3D = Homography3D Mat deriving (Eq, Show, Read)

instance Shaped Homography3D where
    type Shape Homography3D = Dim4x4
    toArray (Homography3D m) = m
    unsafeFromArray = Homography3D

-- | backprojection point -> ray
newtype InvCamera = InvCamera (T.Tensor Double) deriving (Eq, Show)


newtype Conic = Conic Mat deriving (Eq, Show, Read)

instance Shaped Conic where
    type Shape Conic = Dim3x3
    toArray (Conic m) = m
    unsafeFromArray = Conic


newtype Quadric = Quadric Mat deriving (Eq, Show, Read)

instance Shaped Quadric where
    type Shape Quadric = Dim4x4
    toArray (Quadric m) = m
    unsafeFromArray = Quadric

newtype DualConic = DualConic Mat deriving (Eq, Show, Read)

instance Shaped DualConic where
    type Shape DualConic = Dim3x3
    toArray (DualConic m) = m
    unsafeFromArray = DualConic

newtype DualQuadric = DualQuadric Mat deriving (Eq, Show, Read)

instance Shaped DualQuadric where
    type Shape DualQuadric = Dim4x4
    toArray (DualQuadric m) = m
    unsafeFromArray = DualQuadric


---------------------------------------------------------------------

class Transformable t x
  where
    type TResult t x :: *
    apTrans :: t -> x -> TResult t x
    infixl 2 <|
    (<|) :: t -> x -> TResult t x
    (<|) = apTrans
    infixl 2 ◁  -- 25c1
    (◁) :: t -> x -> TResult t x
    (◁) = apTrans

apMat :: (Vectorlike a, Vectorlike b, Matrixlike t) => (Mat -> Mat) -> t -> [a] -> [b]
apMat _ _ [] = []
apMat g h xs = (map unsafeFromVector . toRows) . (<> (g.trans) (toMatrix h)) . fromRows . (map toVector) $ xs

instance Transformable Homography [HPoint]
  where
    type TResult Homography [HPoint] = [HPoint]
    apTrans = apMat id

instance Transformable Homography HPoint
  where
    type TResult Homography HPoint = HPoint
    apTrans t x = let [y] = apTrans t [x] in y


instance Transformable Homography [Point]
  where
    type TResult Homography [Point] = [Point]
    apTrans h = map inhomog . apTrans h . map homog -- FIXME

instance Transformable Homography Point
  where
    type TResult Homography Point = Point
    apTrans t x = let [y] = apTrans t [x] in y


instance Transformable Homography [HLine]
  where
    type TResult Homography [HLine] = [HLine]
    apTrans = apMat (inv.trans)

instance Transformable Homography HLine
  where
    type TResult Homography HLine = HLine
    apTrans t x = let [y] = apTrans t [x] in y


instance Transformable Homography Conic
  where
    type TResult Homography Conic = Conic
    apTrans (Homography t) (Conic c) = Conic (trans it <> c <> it)
      where
        it = inv t

instance Transformable Homography [Conic]
  where
    type TResult Homography [Conic] = [Conic]
    apTrans t = map (apTrans t)


instance Transformable Homography DualConic
  where
    type TResult Homography DualConic = DualConic
    apTrans (Homography t) (DualConic c) = DualConic (t <> c <> trans t)

instance Transformable Homography [DualConic]
  where
    type TResult Homography [DualConic] = [DualConic]
    apTrans t = map (apTrans t)




instance Transformable Homography3D [HPoint3D]
  where
    type TResult Homography3D [HPoint3D] = [HPoint3D]
    apTrans = apMat id

instance Transformable Homography3D HPoint3D
  where
    type TResult Homography3D HPoint3D = HPoint3D
    apTrans t x = let [y] = apTrans t [x] in y


instance Transformable Homography3D [Point3D]
  where
    type TResult Homography3D [Point3D] = [Point3D]
    apTrans h = map inhomog . apTrans h . map homog -- FIXME

instance Transformable Homography3D Point3D
  where
    type TResult Homography3D Point3D = Point3D
    apTrans t x = let [y] = apTrans t [x] in y



instance Transformable Camera [HPoint3D]
  where
    type TResult Camera [HPoint3D] = [HPoint]
    apTrans = apMat id

instance Transformable Camera HPoint3D
  where
    type TResult Camera HPoint3D = HPoint
    apTrans t x = let [y] = apTrans t [x] in y


instance Transformable Camera [Point3D]
  where
    type TResult Camera [Point3D] = [Point]
    apTrans h = map inhomog . apTrans h . map homog -- FIXME

instance Transformable Camera Point3D
  where
    type TResult Camera Point3D = Point
    apTrans t x = let [y] = apTrans t [x] in y




instance Transformable InvCamera HPoint
  where
    type TResult InvCamera HPoint = HLine3D
    apTrans (InvCamera t) p = HLine3D (UT.asMatrix r)
      where
        r = t T.! "rjk" * toTensor p T.! "r"

instance Transformable InvCamera [HPoint]
  where
    type TResult InvCamera [HPoint] = [HLine3D]
    apTrans t = map (apTrans t)



---------------------------------------------------------------------

class BackTransformable t x
  where
    type BResult t x :: *
    bTrans :: t -> x -> BResult t x
    infixl 2 |>
    (|>) :: x -> t -> BResult t x
    (|>) = flip bTrans
    infixl 2 ▷ -- 25b7
    (▷) :: x -> t -> BResult t x
    (▷) = flip bTrans


instance BackTransformable Homography [HLine]
  where
    type BResult Homography [HLine] = [HLine]
    bTrans = apMat trans

instance BackTransformable Homography3D [HPlane]
  where
    type BResult Homography3D [HPlane] = [HPlane]
    bTrans = apMat trans

instance BackTransformable Camera [HLine]
  where
    type BResult Camera [HLine] = [HPlane]
    bTrans = apMat trans

---------------------------------------------------------------------



class Composable s t
  where
    type s :.: t :: *
    compTrans :: s -> t -> s :.: t  -- s . t
    (·) :: s -> t -> s :.: t
    infixr 8  ·
    (·) = compTrans
    (⊙) :: s -> t -> s :.: t
    infixr 8  ⊙ -- utf8 2299
    (⊙) = compTrans

instance Composable Homography Homography
  where
    type Homography :.: Homography = Homography
    compTrans s t = unsafeFromMatrix (toMatrix s <> toMatrix t)

instance Composable Homography3D Homography3D
  where
    type Homography3D :.: Homography3D = Homography3D
    compTrans s t = unsafeFromMatrix (toMatrix s <> toMatrix t)

instance Composable Camera Homography3D
  where
    type Camera :.: Homography3D = Camera
    compTrans s t = unsafeFromMatrix (toMatrix s <> toMatrix t)

instance Composable Homography Camera
  where
    type Homography :.: Camera = Camera
    compTrans s t = unsafeFromMatrix (toMatrix s <> toMatrix t)

---------------------------------------------------------------------


class Invertible t
  where
    type Inv t :: *
    invTrans :: t -> Inv t

instance Invertible Homography
  where
    type Inv Homography = Homography
    invTrans (Homography h) = Homography (inv h)

instance Invertible Homography3D
  where
    type Inv Homography3D = Homography3D
    invTrans (Homography3D h) = Homography3D (inv h)

instance Invertible Camera
  where
    type Inv Camera = InvCamera
    invTrans c = InvCamera t
      where
        tc = toTensor c
        t = eps3 T.! "ijk" * tc T.! "ip" * tc T.! "jq" * eps4 T.! "pqrs"

eps3 :: E.Tensor Double
eps3 = T.cov (E.leviCivita 3)

eps4 :: E.Tensor Double
eps4 = T.contrav (E.leviCivita 4)

-----------------------------------------------------------------------


class Inhomog x
  where
    type HResult x :: *
    homog :: x -> HResult x

instance Inhomog Point
  where
    type HResult Point = HPoint
    homog (Point x y) = HPoint x y 1

instance Inhomog Point3D
  where
    type HResult Point3D = HPoint3D
    homog (Point3D x y z) = HPoint3D x y z 1

instance (Num (Vector t), Container Vector t) => Inhomog (Vector t)
  where
    type HResult (Vector t) = Vector t
    homog v = vjoin [v,1]

instance (Num (Vector t), Container Vector t) =>  Inhomog (Matrix t)
  where
    type HResult (Matrix t) = Matrix t
    homog m = fromBlocks [[m , 1 ]]

instance (Show (UT.NArray i x), UT.Compat i, UT.Coord x) => Inhomog (UT.NArray i x) where
  type HResult (UT.NArray i x) = UT.Name -> UT.NArray i x
  homog t n = ((++[1]) `UT.onIndex` n) t


class Homog x
  where
    type IHResult x :: *
    inhomog :: x -> IHResult x

instance Homog HPoint
  where
    type IHResult HPoint = Point
    inhomog (HPoint x y w) = Point (x/w) (y/w)

instance Homog HPoint3D
  where
    type IHResult HPoint3D = Point3D
    inhomog (HPoint3D x y z w) = Point3D (x/w) (y/w) (z/w)

instance (Num (Vector t), Container Vector t) => Homog (Vector t)
  where
    type IHResult (Vector t) = Vector t
    inhomog v = subVector 0 d (v / scalar (v@>d))
      where
        d = dim v - 1

instance (Num (Vector t), Container Vector t) => Homog (Matrix t)
  where
    type IHResult (Matrix t) = Matrix t
    inhomog m = takeColumns (c-1) (m / dropColumns (c-1) m)
      where
        c = cols m

instance (Show (UT.NArray i x), UT.Compat i, UT.Coord x) => Homog (UT.NArray i x) where
  type IHResult (UT.NArray i x) = UT.Name -> UT.NArray i x
  inhomog t n = (init `UT.onIndex` n) t / last (UT.parts t n)

------------------------------------------------------------------------

class Tensorial x where
    toTensor :: x -> T.Tensor Double


instance Tensorial HPoint where
    toTensor (HPoint x y w) = T.vector [x,y,w]

instance Tensorial HPoint3D where
    toTensor (HPoint3D x y z w) = T.vector [x,y,z,w]

instance Tensorial HLine where
    toTensor (HLine a b c) = T.covector [a,b,c]

instance Tensorial HPlane where
    toTensor (HPlane a b c d) = T.covector [a,b,c,d]

instance Tensorial HLine3D where
    toTensor (HLine3D m) = UT.fromMatrix T.Contra T.Contra m

instance Tensorial Homography where
    toTensor (Homography h) = UT.fromMatrix T.Contra T.Co h

instance Tensorial Homography3D where
    toTensor (Homography3D h) = UT.fromMatrix T.Contra T.Co h

instance Tensorial Camera
  where
    toTensor (Camera c) = UT.fromMatrix T.Contra T.Co c

instance Tensorial InvCamera
  where
    toTensor (InvCamera t) = t

---------------------------------------------------------------------

class Meet s t where
    type s :\/: t :: *
    meet :: s -> t -> s :\/: t

class Join s t where
    type s :/\: t :: *
    gjoin :: s -> t -> s :/\: t

-- line line


crossMat :: Vec -> Mat
crossMat v = (3><3) [ 0,-c, b,
                      c, 0,-a,
                     -b, a, 0]
    where a = v@>0
          b = v@>1
          c = v@>2

meetLines :: HLine -> HLine -> HPoint
meetLines l m = HPoint (v@>0) (v@>1) (v@>2) where v = crossMat (toVector l) <> (toVector m)

instance Meet HLine HLine where
    type HLine :\/: HLine = HPoint
    meet = meetLines

-- point point

joinPoints :: HPoint -> HPoint -> HLine
joinPoints p q = HLine (v@>0) (v@>1) (v@>2) where v = crossMat (toVector p) <> (toVector q)

instance Join HPoint HPoint where
    type HPoint :/\: HPoint = HLine
    gjoin = joinPoints

instance Join Point Point where
    type Point :/\: Point = HLine
    gjoin = gjoin `on` homog

instance Join HPoint3D HPoint3D where
    type HPoint3D :/\: HPoint3D = HLine3D
    gjoin p q = HLine3D (UT.asMatrix t)
      where
        t = ((E./\) `on` toTensor) p q

instance Join Point3D Point3D where
    type Point3D :/\: Point3D = HLine3D
    gjoin = gjoin `on` homog

-- point line

instance Join HPoint3D HLine3D where
    type HPoint3D :/\: HLine3D = HPlane
    gjoin p q = unsafeFromVector . UT.asVector . E.dual $ t
      where
        t =  toTensor p   E./\   toTensor q

instance Join Point3D HLine3D where
    type Point3D :/\: HLine3D = HPlane
    gjoin p q = gjoin (homog p) q

instance Join HLine3D HPoint3D where
    type HLine3D :/\: HPoint3D = HPlane
    gjoin q p = gjoin p q

instance Join HLine3D Point3D where
    type HLine3D :/\: Point3D = HPlane
    gjoin q p = gjoin p q

-- line plane

instance Meet HPlane HLine3D where
    type HPlane :\/: HLine3D = HPoint3D
    meet p l = unsafeFromVector $ UT.asVector $ E.dual ((E.switch . toTensor $ p) E./\ E.dual (toTensor l))

instance Meet HLine3D HPlane where
    type HLine3D :\/: HPlane = HPoint3D
    meet l p = meet p l

-- plane plane

instance Meet HPlane HPlane where
    type HPlane :\/: HPlane = HLine3D
    meet p q = HLine3D . UT.asMatrix . E.dual $ (E.switch . toTensor $ p) E./\ (E.switch . toTensor $ q)

--------------------------------------------------------------------------------

data Polyline = Closed { polyPts :: [Point] }
              | Open   { polyPts :: [Point] } deriving (Show,Read)

instance Transformable Homography Polyline
  where
    type TResult Homography Polyline = Polyline
    apTrans h (Closed ps) = Closed (apTrans h ps)
    apTrans h (Open ps)   = Open   (apTrans h ps)


--------------------------------------------------------------------------------

newtype Polygon = Polygon { polygonNodes :: [Point] }
           deriving (Show,Read)

instance Transformable Homography Polygon
  where
    type TResult Homography Polygon = Polygon
    apTrans h (Polygon ps) = Polygon (apTrans h ps)

--------------------------------------------------------------------------------

data Segment = Segment !Point !Point
  deriving (Show)

instance Storable Segment where
  sizeOf _ = 2*sizeOf (undefined::Point)
  alignment _ = alignment (undefined::Point)
  
  peek p = do
    let pb = castPtr p
    Segment <$> peekElemOff pb 0 <*> peekElemOff pb 1
  
  poke p (Segment a b) = do
    let pb = castPtr p
    pokeElemOff pb 0 a
    pokeElemOff pb 1 b

--------------------------------------------------------------------------------

-- | linear interpolation between two points
interPoint :: Double -> Point -> Point -> Point
interPoint β (Point x1 y1) (Point x2 y2) = Point x3 y3
  where
    x3 = (1-β)*x1 + β*x2
    y3 = (1-β)*y1 + β*y2


-- | vector normal to a segment (to the "left")
normalSegment :: Segment -> Vect2D
normalSegment (Segment (Point x1 y1) (Point x2 y2)) = Vect2D (-dy/d) (dx/d)
  where
    dx = x2-x1
    dy = y2-y1
    d = sqrt (dx**2+dy**2)

-- | The length of a segment.
segmentLength :: Segment -> Double
segmentLength (Segment e1 e2) = distPoints e1 e2

-- | Euclidean distance between two points
distPoints :: Point -> Point -> Double
distPoints (Point a b) (Point x y) = sqrt $ (a-x)^(2::Int)+(b-y)^(2::Int)

bounding :: Polyline -> Polyline
bounding p = Closed [Point x2 y2, Point x1 y2, Point x1 y1, Point x2 y1]
  where
    x1 = minimum xs
    x2 = maximum xs
    y1 = minimum ys
    y2 = maximum ys
    xs = map px (polyPts p)
    ys = map py (polyPts p)
    px (Point x _) = x
    py (Point _ y) = y

--------------------------------------------------------------------------------

cosAngleSegments :: Segment -> Segment -> Double
cosAngleSegments (Segment p q) (Segment p' q') = ca
  where
     Point x0 y0 = p
     Point x1 y1 = q
     Point x0' y0' = p'
     Point x1' y1' = q'
     ux = x1-x0
     uy = y1-y0
     vx = x1'-x0'
     vy = y1'-y0'
     u2 = ux*ux+uy*uy
     v2 = vx*vx+vy*vy
     uv = ux*vx+uy*vy
     ca = uv/(sqrt (abs u2)*sqrt (abs v2))

--------------------------------------------------------------------------------

asSegments :: Polyline -> [Segment]
asSegments (Open ps') = zipWith Segment ps' (tail ps')
asSegments (Closed ps) = asSegments $ Open $ ps++[head ps]

--------------------------------------------------------------------------------

isLeft :: Point -> Point -> Point -> Bool
isLeft (Point x1 y1) (Point x2 y2) (Point x3 y3) =
    (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1) > 0

--------------------------------------------------------------------------------

-- compact expression from http://paulbourke.net/geometry/lineline2d/
segmentIntersection :: Segment -> Segment -> Maybe Point
segmentIntersection (Segment (Point x1 y1) (Point x2 y2)) (Segment (Point x3 y3) (Point x4 y4)) = r
  where
    d = (y4-y3)*(x2-x1)-(x4-x3)*(y2-y1)
    u = ((x4-x3)*(y1-y3)-(y4-y3)*(x1-x3))/d
    v = ((x2-x1)*(y1-y3)-(y2-y1)*(x1-x3))/d
    ok = d /= 0 && 0 < u && u <= 1 && 0 < v && v <= 1
    x = x1 + u*(x2-x1)
    y = y1 + u*(y2-y1)
    r | ok = Just (Point x y)
      | otherwise = Nothing

--------------------------------------------------------------------------------

intersectionLineSegment :: HLine -> Segment -> Maybe Point
intersectionLineSegment l (Segment p1 p2) | ok        = Just p
                                          | otherwise = Nothing
  where
    p = inhomog (meet l (gjoin p1 p2))
    ok = (toVector p - toVector p1) `udot` (toVector p - toVector p2) < 0

--------------------------------------------------------------------------------

-- | signed area of a polygon
orientation :: Polygon -> Double
orientation (Polygon []) = error "orientation requires at least two points"
orientation (Polygon ps@(p:_)) = -0.5 * go ps
  where
    go [] = error "orientation requires at least two points"
    go [q] = f q p
    go (a:r@(b:_)) = f a b + go r
    f (Point x1 y1) (Point x2 y2) = x1*y2-x2*y1

polygonSides :: Polygon -> [Segment]
polygonSides = asSegments . Closed . polygonNodes

--------------------------------------------------------------------------------

-- | a list of vectors compactly stored as rows of a matrix
type DMat = Matrix Double

