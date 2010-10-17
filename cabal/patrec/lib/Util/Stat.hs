-----------------------------------------------------------------------------
{- |
Module      :  Util.Stat
Copyright   :  (c) Alberto Ruiz 2006-10
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Statistical characterization of multivariate samples and Principal Component Analysis.

-}
-----------------------------------------------------------------------------

module Util.Stat
( Stat(..)
, stat

, Codec(..)

, PCARequest(..)
, pca

, icaTrans, ica1, ica2

, robustLocation
) where

import Numeric.LinearAlgebra hiding (eigenvalues)
import Data.List(transpose,sortBy,minimumBy)
import Data.Function(on)
import Util.Misc(Vec,Mat,(//),(&),sqr)
import Util.Optimize(optimize)

mean :: Mat -> Vec
mean m = ones <> m
    where r = rows m
          k = 1 / fromIntegral r
          ones = constant k r

-- | 1st and 2nd order statistics and other useful information extracted from a multivariate sample, where observations are given as rows of a matrix.

data Stat = Stat
    { meanVector              :: Vec
    , covarianceMatrix        :: Mat
    , eigenvalues             :: Vec
    , eigenvectors            :: Mat
    , invCov                  :: Mat
    , whitenedData            :: Mat
    , whiteningTransformation :: Mat -- ^ homogeneous transformation which includes centering
    , whitener                :: Mat
    , varianceVector          :: Vec
    , normalizedData          :: Mat  -- ^ 0 mean and 1 std each var
    , normalize               :: Vec -> Vec 
    , unnormalize             :: Vec -> Vec
    }

-- | Creates a 'Stat' structure from a matrix. Of course, due to laziness, only the fields required by the particular application will be actually computed.
stat :: Mat -> Stat
stat x = s where
    m = mean x
    xc = x - asRow m
    c = (trans xc <> xc) / fromIntegral (rows x -1)
    (l,v') = eigSH' c
    v = trans v'
    lastrow = asRow $ fromList $ replicate (cols x) 0 ++[1.0::Double]
    w = diag (1/sqrt l) <> v
    n = rows x
    n' = fromIntegral n / fromIntegral (n-1)
    vars = scalar n' * mean (xc*xc)
    d = sqrt vars
    f z = (z-m)/d
    g z = m + z*d
    s = Stat { meanVector = m
             , covarianceMatrix = c
             , eigenvalues = l
             , eigenvectors = v
             , invCov = inv c
             , whitener = w
             , whiteningTransformation = w & asColumn (-w <> m) //
                                         lastrow
             , whitenedData = xc <> trans w
             , varianceVector = vars
             , normalizedData = (x - asRow m) / asRow (sqrt vars)
             , normalize = f
             , unnormalize = g
             }


----------------------------------------------------------------------

-- | This structure contains functions to encode and decode individual vectors (or collection of vectors packed into a matrix) obtained by some suitable criterion (e.g. 'pca').
data Codec =
    Codec { encodeVector :: Vec -> Vec
          , decodeVector :: Vec -> Vec
          , encodeMatrix :: Mat -> Mat
          , decodeMatrix :: Mat -> Mat
          , encodeList   :: [Double] -> [Double]
          , decodeList   :: [Double] -> [Double]
}

-- | Two ways to specify the amount of 'pca' compression.
data PCARequest = ReconstructionQuality Double -- ^ ratio of variance
                | SigmaPercent Double          -- ^ ratio of standard deviation
                | NewDimension Int             -- ^ number of desired components

-- | Given the desired compression criterion, from the 'Stat'istics of a dataset it creates a linear 'Codec' based on principal component analysis  (which maximizes mean squared reconstruction error).
pca :: PCARequest -> Stat -> Codec

pca (NewDimension n) st =
    Codec { encodeVector = encv
          , decodeVector = decv
          , encodeMatrix = \x -> (x - asRow m) <> trans vp
          , decodeMatrix = \y -> (y <> vp) + asRow m
          , encodeList = toList . encv . fromList
          , decodeList = toList . decv . fromList
} where
    encv x = vp <> (x - m)
    decv x = (x <> vp) + m
    vp = takeRows n (eigenvectors st)
    m = meanVector st

pca (ReconstructionQuality prec) st = pca (NewDimension n) st where
    s = toList (eigenvalues st)
    n = 1 + (length $ fst $ span (< (prec'*sum s)) $ cumSum s)
    cumSum = tail . scanl (+) 0.0
    prec' = if prec <=0.0 || prec >= 1.0
                then error "the reconstruction quality must be 0<prec<1"
                else prec

pca (SigmaPercent p) st = pca (ReconstructionQuality $ 1-sqr(1-p/100)) st

{- | PedroE's algorithm. For example, with @dist x y = abs (x-y)@ we have:

@> robustLocation dist [1,2,3,11,12,13,14,15::Double]
[(1.0,0.0),(1.0,1.0),(2.0,1.0),(12.0,2.0),(13.0,2.0),(11.0,8.0),(11.0,9.0),(11.0,10.0)]@

-}
robustLocation :: Ord b => (a -> a -> b) -> [a] -> [(a,b)]
robustLocation dis l = mins where
    mins = map (minimumBy (compare `on` snd)) dst
    dst = transpose ds
    ds = map getdis l
    getdis p = sortBy (compare `on` snd) [(p, dis p y) | y<-l]

------------------------------------------------------------

-- | Independent component analysis (very experimental implementation)
icaTrans :: (Mat -> Mat -> Mat) -- ^ update method (e.g. ica1 or ica2 0.01
         -> Mat -- ^ whitened data set
         -> (Mat, [Double]) -- ^ transformation matrix to independent components and negentropy evolution)
icaTrans met d = (fromRows . sortBy (compare `on` q) . toRows $ w, errs)
  where
    cost w' = negentropy' kurt2 (d <> trans w')
    update w' = met w' d
    (w,errs) = optimize 0 0.01 20 update cost (ident (cols d))
    q wi = - dif kurt2 (d <> wi)
    negentropy' fun  = sum . map (dif fun) . toColumns
    dif (f,k) x = sqr(sumElements (f x) / fromIntegral (dim x) - k)
    kurt2 = ((** 4),3)

covar :: Mat -> Mat -> Mat
covar a b = flip scale (trans a <> b) (recip $ fromIntegral (rows a))

-- | (taken from Vivian McPhail's fastICA in hstatistics)
ica1 :: Mat -> Mat -> Mat
ica1 w d = ortho (w + dw) where
    g' u  = 8*u**3*(u**4-3)
    g'' u = 8*u**2*(7*u**4-9)
    y = d <> trans w
    gy = g' y
    mg'y = mean (g'' y)
    b = -mean (y * gy)
    a = -1/(b-mg'y)
    cov = covar gy y
    dw = diag a <> (diag b + cov) <> w
    ortho m = let (u,_,v) = svd m in u <> trans v

-- | simple gradient optimization
ica2 :: Double -> Mat -> Mat -> Mat
ica2 alpha w d = ortho (w + scalar alpha*dw)
  where
    g x = x**4
    g' x = 4*x**3
    k = 3
    y = d <> trans w
    dw = diag (2*(mean (g y) - k)) <> covar (g' y) d
    ortho m = let (u,_,v) = svd m in u <> trans v
