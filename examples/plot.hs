import GSL

sombrero n = mesh (f x y) where 
    (x,y) = meshdom range range
    range = linspace n (-2,2)
    f x y = exp (-r2) * cos (2*r2) where 
        r2 = x*x+y*y

f x =  sin x + 0.5 * sin (5*x)

gaussianPDF = erf_Z
cumdist x = 0.5 * (1+ erf (x/sqrt 2))

main = do
    let x = linspace 1000 (-4,4)
    hplot [f x]
    hplot [x, vmap cumdist x,  vmap gaussianPDF x]
    sombrero 40