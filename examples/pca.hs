-- Principal component analysis

import GSL
   
-- vector with the mean value of the columns of a matrix
mean x = sumCols x / fromIntegral (rows x)

-- covariance matrix of a list of observations as rows of a matrix
cov x = (trans xc <> xc) / fromIntegral (rows x -1) 
    where xc = center x
          center m = m - constant 1 (rows m) `outer` mean m
   
takeRows n = fromRows . take n . toRows   
   
pca :: Int -> M -> (V -> V , V -> V)    
pca n dataSet = (encode,decode)    
  where    
    encode x = vp <> (x - m)
    decode x = x <> vp + m
    m = mean dataSet
    c = cov dataSet
    (_,v) = eig c
    vp = takeRows n (trans v)
    
main = do
    m <- gslReadMatrix "/home/alberto/space/data/mnist.txt" (5000,785)
    let xs = subMatrix 0 (rows m-1) 0 (cols m -2) m
    let x = toRows xs !! 4  -- an arbitrary test vector
    let (pe,pd) = pca 10 xs
    let y = pe x
    disp 2 y
    print $ norm (x - pd y) / norm x
