module Contours.GNS(
    GN,
    prepareGNS,
    stepGNS)
where

import Util.Geometry
import Contours.Base
import Contours.Fourier
import Util.Homogeneous
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util(col,(?),(¿),diagl)
import Util.Misc(rotateLeft,degree)
-- import Vision(kgen,projectionAt',cameraModelOrigin)
import Util.Rotation
import Numeric.GSL.Differentiation


feat :: Polyline -> Int -> Double
feat cont = h
  where
    fou = fourierPL cont
    g = normalizeStart fou

    h k = (sel . g) j
      where
        j = sgn * ((k + 2) `div` 4)
        sel | odd k     = imagPart
            | otherwise = realPart
        sgn | (k+2) `mod` 4 > 1 = -1
            | otherwise         = 1


type GN = (Vector Double, Matrix Double, Polyline -> Vector Double)

prepareGNS :: Int -> Polyline -> GN
prepareGNS n prt = (f0,j0,fun)
  where
    fun tgt = fromList (map (feat tgt) dimfeat)
    f0 = fun prt
    trans k xs = feat (transPol (mktP xs) prt) k
    j0 = jacobian (map trans dimfeat) zerot
    dimfeat = [0..n]
    zerot = replicate 8 0


stepGNS :: GN -> Polyline -> Polyline
stepGNS (f0,j0,fun) tgt = res
  where
    err = fun tgt - f0
    dx = (trans j0 <> j0) <\> (trans j0 <> err)
    res = transPol (inv (mktP (toList dx))) tgt

--------------------------------------------------------------------------------

jacobian :: [[Double] -> Double] -> [Double] -> Matrix Double
jacobian fs xs = fromLists $ map (\f -> gradient f xs) fs

gradient f v = [partialDerivative k f v | k <- [0 .. length v -1]]

partialDerivative n f v = fst (derivCentral 0.01 g (v!!n)) where
    g x = f (concat [a,x:b])
    (a,_:b) = splitAt n v

--------------------------------------------------------------------------------

mktP [a,d,c,b,e,f,g,h] = (3><3) [ 1+a, c,   e,
                                  b  , 1+d, f,
                                  g  ,   h, 1] :: Matrix Double

--------------------------------------------------------------------------------
{-

refinePose :: Int -> Matrix Double -> Polyline -> Polyline -> [Matrix Double]
refinePose n cam0 tgt prt = map model (iterate work zerot)
  where
    model = projectionAt' (cameraModelOrigin Nothing cam0) . fromList
    zerot = replicate 6 0
    hfeats c = [ \h -> feat (transPol h c) k | k <- [0..n] ]
    f0 = fromList $ map ($ ident 3) (hfeats tgt)
    feats = map (. ((<> diagl[-1,1,1]) . (¿[0,1,3]) .  model)) (hfeats prt)
    fun xs = fromList $ map ($ xs) feats
    jac    = jacobian feats
    
    work xs = zipWith (+) xs (toList dxs)
      where
        err = f0 - fun xs
        j = jac xs
        dxs = (trans j <> j) <\> (trans j <> err)
        xs' = zipWith (+) xs (toList dxs)
        info = (norm2 (f0 - fun xs - j<>dxs), norm2 (f0 - fun xs'))
-}

