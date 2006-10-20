{- Principal component analysis
  We work with the mnist.txt datafile automatically downloaded by tests.hs
-}

import GSL

type Vec = Vector Double
type Mat = Matrix Double

sumColumns m = constant 1 (rows m) <> m   

-- Vec with the mean value of the columns of a Mat
mean x = sumColumns x / fromIntegral (rows x)

-- covariance Mat of a list of observations as rows of a Mat
cov x = (trans xc <> xc) / fromIntegral (rows x -1)
    where xc = center x
          center m = m - constant 1 (rows m) `outer` mean m

-- creates the compression and decompression functions from the desired number of components
pca :: Int -> Mat -> (Vec -> Vec , Vec -> Vec)
pca n dataSet = (encode,decode)
  where    
    encode x = vp <> (x - m)
    decode x = x <> vp + m
    m = mean dataSet
    c = cov dataSet
    (_,v) = eigS c
    vp = takeRows n (trans v)

main = do
    m <- fromFile "../apps/examples/mnist.txt" (5000,785)
    let xs = takeColumns (cols m -1) m -- the last column is the digit type (class label)
    let x = toRows xs !! 4  -- an arbitrary test Vec
    let (pe,pd) = pca 10 xs
    let y = pe x
    print y  -- compressed version
    print $ norm (x - pd y) / norm x --reconstruction quality
