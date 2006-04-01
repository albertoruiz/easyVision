-- MSE polynomial model using the pseudoinverse
import GSL

expand :: Int -> Vector -> Matrix
expand n x = fromColumns $ map (x^) [1 .. n] 

polynomialModel :: Matrix -> Int -> (Vector -> Vector)
polynomialModel d n = f where
    f z = expand n z <> ws       
    ws  = pinv a <> b            
    [x,b] = toColumns d  
    a = expand n x
    
main = do
    d <- fromFile "examples/data.txt"
    let [x,y] = toColumns d
    let pol = polynomialModel d
    let view = [x, y, pol 1 x, pol 2 x, pol 3 x]   
    disp 3 $ fromColumns view   
    hplot view
