-- initial comprobation for the polynomial model example
import GSL

prepSyst :: Int -> Matrix Double -> (Matrix Double, Vector Double)
prepSyst n d = (a,b) where
    [x,b] = toColumns d
    a = fromColumns $ map (x^) [1 .. n]

readMatrix :: String -> Matrix Double
readMatrix = fromLists . map (map read). map words . lines

main = do
    dat <- readMatrix `fmap` readFile "data.txt"
    let (a,b) = prepSyst 3 dat
    putStr "Coefficient matrix:\n"
    dispR 3 a
    putStr "Desired values:\n"
    print b
    putStr "\nLeast Squares Solution:\n"
    print $ pinv a <> b
