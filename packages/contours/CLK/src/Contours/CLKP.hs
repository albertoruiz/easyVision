module Contours.CLKP (
    gendata,
    deltaSegment,
    poseStep
)where

import Numeric.LinearAlgebra.HMatrix hiding (hess)
import Util.Geometry
import Contours.Base
import Util.Rotation
import Util.Homogeneous((!<>))
import Vision(homogZ0)
--import Util.Debug(debug)
import Contours.CLK(diffCont)

type V = Vector ℝ
type M = Matrix ℝ
type DGJ = (V, V, [V])

-- create delta field, gradient, and Jacobian of transformation
gendata :: Polyline -> Matrix Double -> Polyline -> [DGJ]
gendata temp cam obs = concat $ zipWith3 (mkJac cam icam) bases deltas grads
  where
    dif = diffCont obs (transPol cam temp)
    icam = inv cam
    bases  = map fst dif
    deltas = map deltaSegment dif
    grads  = map contgrad dif


mkJac :: M -> M -> Segment -> Segment -> Segment -> [DGJ]
mkJac cam icam (Segment p q) (Segment p' q') (Segment p'' q'') = [go p p' p'', go q q' q'']
  where
    go (Point x0 y0) (Point xr yr) (Point xg yg) =
        ( vector[xr-x0, yr-y0]
        , vector[xg-x0, yg-y0]
        , [ vector[ -y',  x']
          , vector[-x'**2 -1, -x'*y']
          , vector[-x'*y', -y'**2 -1]
          , vector[z', 0]
          , vector[0, z']
          , vector[-x'*z', -y'*z'] ])
      where
        [pt] = toRows $ row [x0,y0] !<> tr icam
        [x,y,z] = toList $ cam #> homog pt
        x' = x/z
        y' = y/z
        z' = 1/z

mkGrad :: DGJ -> V
mkGrad (delta,_,ds) = vector $ map (dot delta) ds

mkHess :: DGJ -> M
mkHess (_,cgrad,ds) = scalar(gx*gx)* xx + scalar(gx*gy)* xy + scalar(gy*gx)* yx + scalar(gy*gy)* yy
  where
    [gx,gy] = toList cgrad
    xx = fromLists [ [ u!0 * v!0 | u <- ds] | v<- ds ]
    xy = fromLists [ [ u!0 * v!1 | u <- ds] | v<- ds ]
    yx = fromLists [ [ u!1 * v!0 | u <- ds] | v<- ds ]
    yy = fromLists [ [ u!1 * v!1 | u <- ds] | v<- ds ]


poseStep :: Polyline -> M -> Polyline -> M
poseStep temp cam obs = cam'
  where
    dat  = gendata temp cam obs
    grad = sum $ map mkGrad dat
    hess = sum $ map mkHess dat
    [p1,p2,p3,p4,p5,p6] = toList $ hess <\> grad
    [r1,r2,t] = toColumns cam
    r3 = cross r1 r2
    r0 = fromColumns [r1,r2,r3]

    r = rot3 (-p1) <> rot2 (-p2) <> rot1 (-p3)
    cam' = r <> homogZ0 ( r0 ¦ asColumn (t+vector[p4,p5,p6]) )


deltaSegment :: (Segment, Double) -> Segment
deltaSegment ((Segment p q),d) = Segment p' q'
  where
    p' = localFrame p q 0 0 d 0
    q' = localFrame p q 0 1 d 0

contgrad :: (Segment, Double) -> Segment
contgrad ((Segment p q),_) = Segment p' q'
  where
    p' = localFrame p q 0 0 1 0
    q' = localFrame p q 0 1 1 0


