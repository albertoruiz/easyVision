import GSL

prepSyst :: Int -> Matrix -> (Matrix , Vector)
prepSyst n d = (a,b) where
    [x,b] = toColumns d
    a = fromColumns $ map (x^) [1 .. n]

main = do
    dat <- fromFile "examples/data.txt"
    let (a,b) = prepSyst 3 dat
    putStr "Coefficient matrix:\n"
    disp 2 a
    putStr "Desired values:\n"
    disp 2 b
    putStr "\nLeast Squares Solution:\n"
    disp 5 $ pinv a <> b
        