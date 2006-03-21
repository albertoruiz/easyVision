import GSL

quad f a b = fst $ integrateQAGS 1E-9 100 f a b  

quad2 f a b g1 g2 = quad h a b
    where h x = quad (f x) (g1 x) (g2 x)

volSphere r = 8 * quad2 (\x y -> sqrt (r*r-x*x-y*y)) 
                        0 r (const 0) (\x->sqrt (r*r-x*x))

main = do
    print $ quad (\x -> 4/(x^2+1)) 0 1
    print $ volSphere 1
    print $ 4/3*pi
    print $ volSphere 2.5
    print $ 4/3*pi*2.5**3
