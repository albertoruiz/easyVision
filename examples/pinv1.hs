import GSL

pinv m = v <> diag s' <> trans u where
    (u,s,v) = svd m
    sl@(g:_) = toList s
    s' = fromList . map rec $ sl
    rec x = if x/g < 1E-10 then 1 else 1/x 

prepSyst :: Int -> M -> (M , V)
prepSyst n d = (a,b) where
    [x,b] = toCols d
    a = fromCols $ map (x^) [1 .. n]

main = do
    dat <- fromFile "examples/data.txt"
    let (a,b) = prepSyst 3 dat
    putStr "Coefficient matrix:\n"
    disp 2 a
    putStr "Desired values:\n"
    disp 2 b
    putStr "\nLeast Squares Solution:\n"
    disp 5 $ pinv a <> b
        