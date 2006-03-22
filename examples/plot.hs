import GSL

f x =  sin x + 0.5 * sin (5*x)

gaussianPDF = erf_Z
cumdist x = 0.5 * (1+ erf (x/sqrt 2))

main = do
    let x = linspace 1000 (-4,4)
    hplot [f x]
    hplot [x, vmap cumdist x,  vmap gaussianPDF x]
