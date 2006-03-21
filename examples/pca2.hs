-- Improved PCA, showing graphics

import GSL
import GSL.Util
   
-- division by an integer
m ./. r = m <> (1/fromIntegral r ::Double)

-- vector with the mean value of the columns of a matrix
mean x = sumCols x ./. (rows x)

-- covariance matrix of a list of observations as rows of a matrix
cov x = (trans xc <> xc) ./. (rows x -1) 
    where xc = center x
          center m = m |-| constant 1 (rows m) `outer` mean m
   
takeRows n = fromRows . take n . toRows   
   
type Stat = (V, [Double], M) 
-- 1st and 2nd order statistics of a dataset (mean, eigenvalues and eigenvectors of cov)  
stat :: M -> Stat   
stat x = (m, toList s, trans v) where   
    m = mean x
    (s,v) = eig (cov x)   
   
pca :: Double -> Stat -> (V -> V , V -> V)          
pca prec (m,s,v) = (encode,decode)    
  where    
    encode x = vp <> (x |-| m)
    decode x = x <> vp |+| m
    vp = takeRows n v    
    n = 1 + (length $ fst $ span (< (prec'*sum s)) $ cumSum s)
    cumSum = tail . scanl (+) 0.0     
    prec' = if prec <=0.0 || prec >= 1.0
                then error "the precision in pca must be 0<prec<1"
                else prec   
    
mnist :: IO M  
mnist = gslReadMatrix "/home/alberto/space/data/mnist.txt" (5000,785) 

shdigit :: V -> IO ()
shdigit v = imshow (reshape 28 ((-1::Double) <> v))

test :: Stat -> Double -> V -> IO ()
test st prec x = do
    let (pe,pd) = pca prec st
    let y = pe x
    print $ size y
    shdigit (pd y)    
    
main = do
    m <- gslReadMatrix "/home/alberto/space/data/mnist.txt" (5000,785)
    let xs = subMatrix 0 (rows m-1) 0 (cols m -2) m
    let x = toRows xs !! 4  -- an arbitrary test vector
    shdigit x
    let st = stat xs
    test st 0.90 x
    test st 0.50 x
