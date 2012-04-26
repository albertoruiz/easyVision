{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module Vision.Apps.Show (
    showVectorField
) where

import Vision.GUI
import ImagProc
import ImagProc.Ipp.Core(fval)
import GHC.Float(float2Double)

--------------------------------------------------------------------------------

autoParam "VectorFieldParam" "vf-" [ ("scale","Double", realParam 1 0 5) 
                                   , ("step", "Int",    intParam 10  1 30) ]

showVectorField :: Renderable a
                => String -> (c -> (a, (ImageFloat, ImageFloat))) -> ITrans c c
showVectorField name f = withParam (,) >>> observe name sh >>> arr snd
  where
    sh (VectorFieldParam {..}, z) = Draw [ Draw x, 
                                           color white ls, color red ps
                                         ]
      where
        (x,vs) = f z
        ls = gradLines step scale vs
        ps = map extreme2 ls


gradLines d s (gx,gy) = ss
  where
    ROI r1 r2 c1 c2 = intersection (theROI gx) (theROI gy)
    sample = [Pixel r c | r <- [r1,r1+d .. r2], c <- [c1,c1+d .. c2] ]
    locs = pixelsToPoints (size gx) sample
    vgx = map (fval gx) sample
    vgy = map (fval gy) sample
    ss = zipWith3 f locs vgx vgy
    f (Point x y) dx dy = Segment (Point x y)
                                  (Point (x + s * float2Double dx)
                                         (y + s * float2Double dy))

--------------------------------------------------------------------------------

