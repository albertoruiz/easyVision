import GSL

expand :: Int -> Vector -> Matrix
expand n x = fromCols $ map (x^) [1 .. n] 

polynomialModel :: Int -> Matrix -> (Vector -> Vector)
polynomialModel n d = f where
    f z = expand n z <> ws       
    ws  = pinv a <> b            
    [x,b] = toCols d  
    a = expand n x
    
main = do
    d <- fromFile "examples/data.txt"
    let x = head (toCols d)
    let f = polynomialModel 3 d
    disp 3 $ fromBlocks [[d, reshape 1 (f x)]]     

