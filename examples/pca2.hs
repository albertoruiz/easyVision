-- Improved PCA, including illustrative graphics
-- You can download the data file from http://dis.um.es/~alberto/material/sp/minst.txt.zip

import GSL
   
sumColumns m = constant 1 (rows m) <> m   
   
-- vector with the mean value of the columns of a matrix
mean x = sumColumns x / fromIntegral (rows x)

-- covariance matrix of a list of observations as rows of a matrix
cov x = (trans xc <> xc) / fromIntegral (rows x -1) 
    where xc = center x
          center m = m - constant 1 (rows m) `outer` mean m
   
takeRows n = fromRows . take n . toRows   
   
type Stat = (Vector, [Double], Matrix) 
-- 1st and 2nd order statistics of a dataset (mean, eigenvalues and eigenvectors of cov)  
stat :: Matrix -> Stat   
stat x = (m, toList s, trans v) where   
    m = mean x
    (s,v) = eig (cov x)   
   
-- creates the compression and decompression functions from the desired reconstruction 
-- quality and the statistics of a data set
pca :: Double -> Stat -> (Vector -> Vector , Vector -> Vector)          
pca prec (m,s,v) = (encode,decode)    
  where    
    encode x = vp <> (x - m)
    decode x = x <> vp + m
    vp = takeRows n v    
    n = 1 + (length $ fst $ span (< (prec'*sum s)) $ cumSum s)
    cumSum = tail . scanl (+) 0.0     
    prec' = if prec <=0.0 || prec >= 1.0
                then error "the precision in pca must be 0<prec<1"
                else prec   
    
mnist :: IO Matrix  
mnist = gslReadMatrix "examples/mnist.txt" (5000,785) 

shdigit :: Vector -> IO ()
shdigit v = imshow (reshape 28 (-v))
--shdigit v = meshOpenGL $ const (reshape 28 v/300)

-- shows the effect of a given reconstruction quality on a test vector
test :: Stat -> Double -> Vector -> IO ()
test st prec x = do
    let (pe,pd) = pca prec st
    let y = pe x
    print $ size y
    shdigit (pd y)    
    
main = do
    m <- mnist
    let xs = subMatrix 0 (rows m-1) 0 (cols m -2) m
    let x = toRows xs !! 4  -- an arbitrary test vector
    shdigit x
    let st = stat xs
    test st 0.90 x
    test st 0.50 x
