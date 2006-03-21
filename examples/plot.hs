import GSL
import Graphics.HGL

r .*. x = (r::Double) <> x

f x =  vmap sin x |+| 0.5 .*. vmap sin (2 .*. x)

main = do
    let x = linspace 100 (-2,2)
    hglPlot [x, vmap erf x]
    plot [vmap sin, f] (0,2*pi) 100    

-------------------------------------------------------------

hglPlot :: [V] -> IO ()
hglPlot [] = return ()
hglPlot [v] = hglPlot [r,v] where r = realVector [1 .. fromIntegral (size v)]
hglPlot [x,y] =  windowHGL [polyline l] where
    l = zip (adaptVector (0,399) x) (adaptVector (299,0) y)

adaptVector:: (Int,Int) -> V -> [Int]
adaptVector (a,b) v = k <> v |+| c // toList // map round
    where mx = maximum vl
          mn = minimum vl
          vl = toList v
          k = if mx == mn then 0 else (fromIntegral b- fromIntegral a)/(mx-mn)
          c = fromIntegral a - k*mn

windowHGL grs = do 
    runGraphics $ withWindow_ "HGL Plot" (400, 300) $ \ w -> do
        drawInWindow w $ overGraphics grs  
        k<-getKey w
        return ()

