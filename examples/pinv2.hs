-- MSE polynomial model using the pseudoinverse
import GSL

expand :: Int -> Vector Double -> Matrix Double
expand n x = fromColumns $ map (x^) [1 .. n]

polynomialModel :: Matrix Double -> Int -> (Vector Double -> Vector Double)
polynomialModel d n = f where
    f z = expand n z <> ws 
    ws  = pinv a <> b
    [x,b] = toColumns d 
    a = expand n x

readMatrix :: String -> Matrix Double
readMatrix = fromLists . map (map read). map words . lines

main = do
    d <- readMatrix `fmap` readFile "data.txt"
    let [x,y] = toColumns d
    let pol = polynomialModel d
    let view = [x, y, pol 1 x, pol 2 x, pol 3 x]
    dispR 2 $ fromColumns view
    print $ fromColumns view
    --hplot view
