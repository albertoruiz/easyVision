-- Principal component analysis
-- You can download the data file from http://dis.um.es/~alberto/material/sp/minst.txt.zip

import GSL
   
sumColumns m = constant 1 (rows m) <> m   
   
-- vector with the mean value of the columns of a matrix
mean x = sumColumns x / fromIntegral (rows x)

-- covariance matrix of a list of observations as rows of a matrix
cov x = (trans xc <> xc) / fromIntegral (rows x -1) 
    where xc = center x
          center m = m - constant 1 (rows m) `outer` mean m
   
-- creates the compression and decompression functions from the desired number of components
pca :: Int -> Matrix -> (Vector -> Vector , Vector -> Vector)    
pca n dataSet = (encode,decode)    
  where    
    encode x = vp <> (x - m)
    decode x = x <> vp + m
    m = mean dataSet
    c = cov dataSet
    (_,v) = eig c
    vp = takeRows n (trans v)
    
main = do
    m <- gslReadMatrix "mnist.txt" (5000,785)
    let xs = takeColumns (cols m -1) m -- the last column is the digit type (class label)
    let x = toRows xs !! 4  -- an arbitrary test vector
    let (pe,pd) = pca 10 xs
    let y = pe x
    disp 2 y  -- compressed version
    print $ norm (x - pd y) / norm x --reconstruction quality
